use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::{AggregatedSpirvAttributes, Entry};
use crate::builder::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::codegen_cx::BindlessDescriptorSets;
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::{
    Capability, Decoration, Dim, ExecutionModel, FunctionControl, StorageClass, Word,
};
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir as hir;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{Instance, Ty, TyKind};
use rustc_span::Span;
use rustc_target::abi::{
    call::{ArgAbi, ArgAttribute, ArgAttributes, FnAbi, PassMode},
    Align, LayoutOf, Size,
};

impl<'tcx> CodegenCx<'tcx> {
    // Entry points declare their "interface" (all uniforms, inputs, outputs, etc.) as parameters.
    // spir-v uses globals to declare the interface. So, we need to generate a lil stub for the
    // "real" main that collects all those global variables and calls the user-defined main
    // function.
    pub fn entry_stub(
        &self,
        instance: &Instance<'_>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        entry_func: SpirvValue,
        name: String,
        entry: Entry,
    ) {
        let span = self.tcx.def_span(instance.def_id());
        let hir_params = {
            let fn_local_def_id = match instance.def_id().as_local() {
                Some(id) => id,
                None => {
                    self.tcx
                        .sess
                        .span_err(span, &format!("Cannot declare {} as an entry point", name));
                    return;
                }
            };
            let fn_hir_id = self.tcx.hir().local_def_id_to_hir_id(fn_local_def_id);
            let body = self.tcx.hir().body(self.tcx.hir().body_owned_by(fn_hir_id));
            body.params
        };
        const EMPTY: ArgAttribute = ArgAttribute::empty();
        for (abi, hir_param) in fn_abi.args.iter().zip(hir_params) {
            match abi.mode {
                PassMode::Direct(_)
                | PassMode::Indirect { .. }
                // plain DST/RTA/VLA
                | PassMode::Pair(
                    ArgAttributes {
                        pointee_size: Size::ZERO,
                        ..
                    },
                    ArgAttributes { regular: EMPTY, .. },
                )
                // DST struct with fields before the DST member
                | PassMode::Pair(
                    ArgAttributes { .. },
                    ArgAttributes {
                        pointee_size: Size::ZERO,
                        ..
                    },
                ) => {}
                _ => self.tcx.sess.span_err(
                    hir_param.ty_span,
                    &format!("PassMode {:?} invalid for entry point parameter", abi.mode),
                ),
            }
        }
        if let PassMode::Ignore = fn_abi.ret.mode {
        } else {
            self.tcx.sess.span_err(
                span,
                &format!(
                    "PassMode {:?} invalid for entry point return type",
                    fn_abi.ret.mode
                ),
            );
        }
        let execution_model = entry.execution_model;
        let fn_id = if execution_model == ExecutionModel::Kernel {
            self.kernel_entry_stub(entry_func, name, execution_model)
        } else {
            self.shader_entry_stub(
                span,
                entry_func,
                &fn_abi.args,
                hir_params,
                name,
                execution_model,
            )
        };
        let mut emit = self.emit_global();
        entry
            .execution_modes
            .iter()
            .for_each(|(execution_mode, execution_mode_extra)| {
                emit.execution_mode(fn_id, *execution_mode, execution_mode_extra);
            });
    }

    pub fn lazy_add_bindless_descriptor_sets(&self) {
        self.bindless_descriptor_sets
            .replace(Some(BindlessDescriptorSets {
                // all storage buffers are compatible and go in set 0
                buffers: self.buffer_descriptor_set(0),

                // sampled images are all compatible in vulkan, so we can overlap them
                sampled_image_1d: self.texture_bindless_descriptor_set(
                    1,
                    rspirv::spirv::Dim::Dim1D,
                    true,
                ),
                sampled_image_2d: self.texture_bindless_descriptor_set(
                    1,
                    rspirv::spirv::Dim::Dim2D,
                    true,
                ),
                sampled_image_3d: self.texture_bindless_descriptor_set(
                    1,
                    rspirv::spirv::Dim::Dim3D,
                    true,
                ),
                // jb-todo: storage images are all compatible so they can live in the same descriptor set too
            }));
    }

    fn buffer_descriptor_set(&self, descriptor_set: u32) -> Word {
        let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);

        let runtime_array_uint =
            SpirvType::RuntimeArray { element: uint_ty }.def(rustc_span::DUMMY_SP, self);

        let buffer_struct = SpirvType::Adt {
            def_id: None,
            size: Some(Size::from_bytes(4)),
            align: Align::from_bytes(4).unwrap(),
            field_types: vec![runtime_array_uint],
            field_offsets: vec![],
            field_names: None,
        }
        .def(rustc_span::DUMMY_SP, self);

        let runtime_array_struct = SpirvType::RuntimeArray {
            element: buffer_struct,
        }
        .def(rustc_span::DUMMY_SP, self);

        let uniform_ptr_runtime_array = SpirvType::Pointer {
            pointee: runtime_array_struct,
        }
        .def(rustc_span::DUMMY_SP, self);

        let mut emit_global = self.emit_global();
        let buffer = emit_global
            .variable(
                uniform_ptr_runtime_array,
                None,
                if self.target.spirv_version() <= (1, 3) {
                    StorageClass::Uniform
                } else {
                    StorageClass::StorageBuffer
                },
                None,
            )
            .with_type(uniform_ptr_runtime_array)
            .def_cx(self);

        emit_global.decorate(
            buffer,
            rspirv::spirv::Decoration::DescriptorSet,
            std::iter::once(Operand::LiteralInt32(descriptor_set)),
        );
        emit_global.decorate(
            buffer,
            rspirv::spirv::Decoration::Binding,
            std::iter::once(Operand::LiteralInt32(0)),
        );

        if self.target.spirv_version() <= (1, 3) {
            emit_global.decorate(
                buffer_struct,
                rspirv::spirv::Decoration::BufferBlock,
                std::iter::empty(),
            );
        } else {
            emit_global.decorate(
                buffer_struct,
                rspirv::spirv::Decoration::Block,
                std::iter::empty(),
            );
        }

        emit_global.decorate(
            runtime_array_uint,
            rspirv::spirv::Decoration::ArrayStride,
            std::iter::once(Operand::LiteralInt32(4)),
        );

        emit_global.member_decorate(
            buffer_struct,
            0,
            rspirv::spirv::Decoration::Offset,
            std::iter::once(Operand::LiteralInt32(0)),
        );

        buffer
    }

    fn texture_bindless_descriptor_set(
        &self,
        descriptor_set: u32,
        dim: rspirv::spirv::Dim,
        sampled: bool,
    ) -> Word {
        let float_ty = SpirvType::Float(32).def(rustc_span::DUMMY_SP, self);

        let image = SpirvType::Image {
            sampled_type: float_ty,
            dim,
            depth: 0,
            arrayed: 0,
            multisampled: 0,
            sampled: if sampled { 1 } else { 0 },
            image_format: rspirv::spirv::ImageFormat::Unknown,
            access_qualifier: None,
        }
        .def(rustc_span::DUMMY_SP, self);

        let sampled_image =
            SpirvType::SampledImage { image_type: image }.def(rustc_span::DUMMY_SP, self);

        let runtime_array_image = SpirvType::RuntimeArray {
            element: sampled_image,
        }
        .def(rustc_span::DUMMY_SP, self);

        let uniform_ptr_runtime_array = SpirvType::Pointer {
            pointee: runtime_array_image,
        }
        .def(rustc_span::DUMMY_SP, self);

        let mut emit_global = self.emit_global();
        let image_array = emit_global
            .variable(uniform_ptr_runtime_array, None, StorageClass::Uniform, None)
            .with_type(uniform_ptr_runtime_array)
            .def_cx(self);

        emit_global.decorate(
            image_array,
            rspirv::spirv::Decoration::DescriptorSet,
            std::iter::once(Operand::LiteralInt32(descriptor_set)),
        );
        emit_global.decorate(
            image_array,
            rspirv::spirv::Decoration::Binding,
            std::iter::once(Operand::LiteralInt32(0)),
        );

        image_array
    }

    fn shader_entry_stub(
        &self,
        span: Span,
        entry_func: SpirvValue,
        arg_abis: &[ArgAbi<'tcx, Ty<'tcx>>],
        hir_params: &[hir::Param<'tcx>],
        name: String,
        execution_model: ExecutionModel,
    ) -> Word {
        let stub_fn = {
            let void = SpirvType::Void.def(span, self);
            let fn_void_void = SpirvType::Function {
                return_type: void,
                arguments: vec![],
            }
            .def(span, self);
            let mut emit = self.emit_global();
            let id = emit
                .begin_function(void, None, FunctionControl::NONE, fn_void_void)
                .unwrap();
            emit.end_function().unwrap();
            id.with_type(fn_void_void)
        };

        let mut op_entry_point_interface_operands = vec![];

        let mut bx = Builder::build(self, Builder::append_block(self, stub_fn, ""));
        let mut call_args = vec![];
        let mut decoration_locations = FxHashMap::default();
        for (entry_arg_abi, hir_param) in arg_abis.iter().zip(hir_params) {
            bx.set_span(hir_param.span);
            self.declare_shader_interface_for_param(
                entry_arg_abi,
                hir_param,
                &mut op_entry_point_interface_operands,
                &mut bx,
                &mut call_args,
                &mut decoration_locations,
            );
        }
        bx.set_span(span);
        bx.call(entry_func, &call_args, None);
        bx.ret_void();

        if self.bindless() {
            self.emit_global().extension("SPV_EXT_descriptor_indexing");
            self.emit_global()
                .capability(Capability::RuntimeDescriptorArray);

            if self.target.spirv_version() > (1, 3) {
                let sets = self.bindless_descriptor_sets.borrow().unwrap();

                op_entry_point_interface_operands.push(sets.buffers);

                //op_entry_point_interface_operands
                //  .push(sets.sampled_image_1d);
                // op_entry_point_interface_operands
                //   .push(sets.sampled_image_2d);
                //op_entry_point_interface_operands
                //.push(sets.sampled_image_3d);
            }
        }

        let stub_fn_id = stub_fn.def_cx(self);
        self.emit_global().entry_point(
            execution_model,
            stub_fn_id,
            name,
            op_entry_point_interface_operands,
        );
        stub_fn_id
    }

    fn infer_param_ty_and_storage_class(
        &self,
        layout: TyAndLayout<'tcx>,
        hir_param: &hir::Param<'tcx>,
        attrs: &AggregatedSpirvAttributes,
    ) -> (Word, StorageClass) {
        // FIXME(eddyb) attribute validation should be done ahead of time.
        // FIXME(eddyb) also take into account `&T` interior mutability,
        // i.e. it's only immutable if `T: Freeze`, which we should check.
        // FIXME(eddyb) also check the type for compatibility with being
        // part of the interface, including potentially `Sync`ness etc.
        let (value_ty, mutbl, is_ref) = match *layout.ty.kind() {
            TyKind::Ref(_, pointee_ty, mutbl) => (pointee_ty, mutbl, true),
            _ => (layout.ty, hir::Mutability::Not, false),
        };
        let spirv_ty = self.layout_of(value_ty).spirv_type(hir_param.ty_span, self);
        // Some types automatically specify a storage class. Compute that here.
        let element_ty = match self.lookup_type(spirv_ty) {
            SpirvType::Array { element, .. } | SpirvType::RuntimeArray { element } => {
                self.lookup_type(element)
            }
            ty => ty,
        };
        let inferred_storage_class_from_ty = match element_ty {
            SpirvType::Image { .. }
            | SpirvType::Sampler
            | SpirvType::SampledImage { .. }
            | SpirvType::AccelerationStructureKhr { .. } => {
                if is_ref {
                    Some(StorageClass::UniformConstant)
                } else {
                    self.tcx.sess.span_err(
                        hir_param.ty_span,
                        &format!(
                            "entry parameter type must be by-reference: `&{}`",
                            layout.ty,
                        ),
                    );
                    None
                }
            }
            _ => None,
        };
        // Storage classes can be specified via attribute. Compute that here, and emit diagnostics.
        let attr_storage_class = attrs.storage_class.map(|storage_class_attr| {
            let storage_class = storage_class_attr.value;

            let expected_mutbl = match storage_class {
                StorageClass::UniformConstant
                | StorageClass::Input
                | StorageClass::Uniform
                | StorageClass::PushConstant => hir::Mutability::Not,

                _ => hir::Mutability::Mut,
            };

            if !is_ref {
                self.tcx.sess.span_fatal(
                    hir_param.ty_span,
                    &format!(
                        "invalid entry param type `{}` for storage class `{:?}` \
                         (expected `&{}T`)",
                        layout.ty,
                        storage_class,
                        expected_mutbl.prefix_str()
                    ),
                )
            }

            match inferred_storage_class_from_ty {
                Some(inferred) if storage_class == inferred => self.tcx.sess.span_warn(
                    storage_class_attr.span,
                    "redundant storage class specifier, storage class is inferred from type",
                ),
                Some(inferred) => self
                    .tcx
                    .sess
                    .struct_span_err(hir_param.span, "storage class mismatch")
                    .span_label(
                        storage_class_attr.span,
                        format!("{:?} specified in attribute", storage_class),
                    )
                    .span_label(
                        hir_param.ty_span,
                        format!("{:?} inferred from type", inferred),
                    )
                    .span_help(
                        storage_class_attr.span,
                        &format!(
                            "remove storage class attribute to use {:?} as storage class",
                            inferred
                        ),
                    )
                    .emit(),
                None => (),
            }

            storage_class
        });
        // If storage class was not inferred nor specified, compute the default (i.e. input/output)
        let storage_class = inferred_storage_class_from_ty
            .or(attr_storage_class)
            .unwrap_or_else(|| match (is_ref, mutbl) {
                (false, _) => StorageClass::Input,
                (true, hir::Mutability::Mut) => StorageClass::Output,
                (true, hir::Mutability::Not) => self.tcx.sess.span_fatal(
                    hir_param.ty_span,
                    &format!(
                        "invalid entry param type `{}` (expected `{}` or `&mut {1}`)",
                        layout.ty, value_ty
                    ),
                ),
            });

        (spirv_ty, storage_class)
    }

    fn declare_shader_interface_for_param(
        &self,
        entry_arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        hir_param: &hir::Param<'tcx>,
        op_entry_point_interface_operands: &mut Vec<Word>,
        bx: &mut Builder<'_, 'tcx>,
        call_args: &mut Vec<SpirvValue>,
        decoration_locations: &mut FxHashMap<StorageClass, u32>,
    ) {
        let attrs = AggregatedSpirvAttributes::parse(self, self.tcx.hir().attrs(hir_param.hir_id));

        // Pre-allocate the module-scoped `OpVariable`'s *Result* ID.
        let var = self.emit_global().id();

        let (value_spirv_type, storage_class) =
            self.infer_param_ty_and_storage_class(entry_arg_abi.layout, hir_param, &attrs);

        // Certain storage classes require an `OpTypeStruct` decorated with `Block`,
        // which we represent with `SpirvType::InterfaceBlock` (see its doc comment).
        // This "interface block" construct is also required for "runtime arrays".
        let is_unsized = self.lookup_type(value_spirv_type).sizeof(self).is_none();
        let is_pair = matches!(entry_arg_abi.mode, PassMode::Pair(..));
        let is_unsized_with_len = is_pair && is_unsized;
        if is_pair && !is_unsized {
            // If PassMode is Pair, then we need to fill in the second part of the pair with a
            // value. We currently only do that with unsized types, so if a type is a pair for some
            // other reason (e.g. a tuple), we bail.
            self.tcx
                .sess
                .span_fatal(hir_param.ty_span, "pair type not supported yet")
        }
        let var_ptr_spirv_type;
        let (value_ptr, value_len) = match storage_class {
            StorageClass::PushConstant | StorageClass::Uniform | StorageClass::StorageBuffer => {
                var_ptr_spirv_type = self.type_ptr_to(
                    SpirvType::InterfaceBlock {
                        inner_type: value_spirv_type,
                    }
                    .def(hir_param.span, self),
                );

                let value_ptr = bx.struct_gep(var.with_type(var_ptr_spirv_type), 0);

                let value_len = if is_unsized_with_len {
                    match self.lookup_type(value_spirv_type) {
                        SpirvType::RuntimeArray { .. } => {}
                        _ => self.tcx.sess.span_err(
                            hir_param.ty_span,
                            "only plain slices are supported as unsized types",
                        ),
                    }

                    // FIXME(eddyb) shouldn't this be `usize`?
                    let len_spirv_type = self.type_isize();
                    let len = bx
                        .emit()
                        .array_length(len_spirv_type, None, var, 0)
                        .unwrap();

                    Some(len.with_type(len_spirv_type))
                } else {
                    if is_unsized {
                        // It's OK to use a RuntimeArray<u32> and not have a length parameter, but
                        // it's just nicer ergonomics to use a slice.
                        self.tcx
                            .sess
                            .span_warn(hir_param.ty_span, "use &[T] instead of &RuntimeArray<T>");
                    }
                    None
                };

                (value_ptr, value_len)
            }
            StorageClass::UniformConstant => {
                var_ptr_spirv_type = self.type_ptr_to(value_spirv_type);

                match self.lookup_type(value_spirv_type) {
                    SpirvType::RuntimeArray { .. } => {
                        if is_unsized_with_len {
                            self.tcx.sess.span_err(
                                hir_param.ty_span,
                                "uniform_constant must use &RuntimeArray<T>, not &[T]",
                            );
                        }
                    }
                    _ => {
                        if is_unsized {
                            self.tcx.sess.span_err(
                                hir_param.ty_span,
                                "only plain slices are supported as unsized types",
                            );
                        }
                    }
                }

                let value_len = if is_pair {
                    // We've already emitted an error, fill in a placeholder value
                    Some(bx.undef(self.type_isize()))
                } else {
                    None
                };

                (var.with_type(var_ptr_spirv_type), value_len)
            }
            _ => {
                var_ptr_spirv_type = self.type_ptr_to(value_spirv_type);

                if is_unsized {
                    self.tcx.sess.span_fatal(
                        hir_param.ty_span,
                        &format!(
                            "unsized types are not supported for storage class {:?}",
                            storage_class
                        ),
                    );
                }

                (var.with_type(var_ptr_spirv_type), None)
            }
        };

        // Compute call argument(s) to match what the Rust entry `fn` expects,
        // starting from the `value_ptr` pointing to a `value_spirv_type`
        // (e.g. `Input` doesn't use indirection, so we have to load from it).
        if let TyKind::Ref(..) = entry_arg_abi.layout.ty.kind() {
            call_args.push(value_ptr);
            match entry_arg_abi.mode {
                PassMode::Direct(_) => assert_eq!(value_len, None),
                PassMode::Pair(..) => call_args.push(value_len.unwrap()),
                _ => unreachable!(),
            }
        } else {
            assert_eq!(storage_class, StorageClass::Input);

            call_args.push(match entry_arg_abi.mode {
                PassMode::Indirect { .. } => value_ptr,
                PassMode::Direct(_) => bx.load(value_ptr, entry_arg_abi.layout.align.abi),
                _ => unreachable!(),
            });
            assert_eq!(value_len, None);
        }

        // FIXME(eddyb) check whether the storage class is compatible with the
        // specific shader stage of this entry-point, and any decorations
        // (e.g. Vulkan has specific rules for builtin storage classes).

        // Emit `OpName` in the simple case of a pattern that's just a variable
        // name (e.g. "foo" for `foo: Vec3`). While `OpName` is *not* suppposed
        // to be semantic, OpenGL and some tooling rely on it for reflection.
        if let hir::PatKind::Binding(_, _, ident, _) = &hir_param.pat.kind {
            self.emit_global().name(var, ident.to_string());
        }

        // Emit `OpDecorate`s based on attributes.
        let mut decoration_supersedes_location = false;
        if let Some(builtin) = attrs.builtin.map(|attr| attr.value) {
            self.emit_global().decorate(
                var,
                Decoration::BuiltIn,
                std::iter::once(Operand::BuiltIn(builtin)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(index) = attrs.descriptor_set.map(|attr| attr.value) {
            if self.bindless() {
                self.tcx.sess.span_fatal(
                    attrs.descriptor_set.unwrap().span,
                    "Can't use #[spirv(descriptor_set)] attribute in bindless mode",
                );
            }

            self.emit_global().decorate(
                var,
                Decoration::DescriptorSet,
                std::iter::once(Operand::LiteralInt32(index)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(index) = attrs.binding.map(|attr| attr.value) {
            if self.bindless() {
                self.tcx.sess.span_fatal(
                    attrs.binding.unwrap().span,
                    "Can't use #[spirv(binding)] attribute in bindless mode",
                );
            }
            self.emit_global().decorate(
                var,
                Decoration::Binding,
                std::iter::once(Operand::LiteralInt32(index)),
            );
            decoration_supersedes_location = true;
        }
        if attrs.flat.is_some() {
            self.emit_global()
                .decorate(var, Decoration::Flat, std::iter::empty());
        }
        if let Some(invariant) = attrs.invariant {
            self.emit_global()
                .decorate(var, Decoration::Invariant, std::iter::empty());
            if storage_class != StorageClass::Output {
                self.tcx.sess.span_err(
                    invariant.span,
                    "#[spirv(invariant)] is only valid on Output variables",
                );
            }
        }

        let is_subpass_input = match self.lookup_type(value_spirv_type) {
            SpirvType::Image {
                dim: Dim::DimSubpassData,
                ..
            } => true,
            SpirvType::RuntimeArray { element: elt, .. }
            | SpirvType::Array { element: elt, .. } => matches!(
                self.lookup_type(elt),
                SpirvType::Image {
                    dim: Dim::DimSubpassData,
                    ..
                }
            ),
            _ => false,
        };
        if let Some(attachment_index) = attrs.input_attachment_index {
            if is_subpass_input && self.builder.has_capability(Capability::InputAttachment) {
                self.emit_global().decorate(
                    var,
                    Decoration::InputAttachmentIndex,
                    std::iter::once(Operand::LiteralInt32(attachment_index.value)),
                );
            } else if is_subpass_input {
                self.tcx
                    .sess
                    .span_err(hir_param.ty_span, "Missing capability InputAttachment");
            } else {
                self.tcx.sess.span_err(
                    attachment_index.span,
                    "#[spirv(input_attachment_index)] is only valid on Image types with dim = SubpassData"
                );
            }
            decoration_supersedes_location = true;
        } else if is_subpass_input {
            self.tcx.sess.span_err(
                hir_param.ty_span,
                "Image types with dim = SubpassData require #[spirv(input_attachment_index)] decoration",
            );
        }

        // Assign locations from left to right, incrementing each storage class
        // individually.
        // TODO: Is this right for UniformConstant? Do they share locations with
        // input/outpus?
        let has_location = !decoration_supersedes_location
            && matches!(
                storage_class,
                StorageClass::Input | StorageClass::Output | StorageClass::UniformConstant
            );
        if has_location {
            let location = decoration_locations
                .entry(storage_class)
                .or_insert_with(|| 0);
            self.emit_global().decorate(
                var,
                Decoration::Location,
                std::iter::once(Operand::LiteralInt32(*location)),
            );
            *location += 1;
        }

        // Emit the `OpVariable` with its *Result* ID set to `var`.
        self.emit_global()
            .variable(var_ptr_spirv_type, Some(var), storage_class, None);

        // Record this `OpVariable` as needing to be added (if applicable),
        // to the *Interface* operands of the `OpEntryPoint` instruction.
        if self.emit_global().version().unwrap() > (1, 3) {
            // SPIR-V >= v1.4 includes all OpVariables in the interface.
            op_entry_point_interface_operands.push(var);
        } else {
            // SPIR-V <= v1.3 only includes Input and Output in the interface.
            if storage_class == StorageClass::Input || storage_class == StorageClass::Output {
                op_entry_point_interface_operands.push(var);
            }
        }
    }

    // Kernel mode takes its interface as function parameters(??)
    // OpEntryPoints cannot be OpLinkage, so write out a stub to call through.
    fn kernel_entry_stub(
        &self,
        entry_func: SpirvValue,
        name: String,
        execution_model: ExecutionModel,
    ) -> Word {
        let (entry_func_return, entry_func_args) = match self.lookup_type(entry_func.ty) {
            SpirvType::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => self.tcx.sess.fatal(&format!(
                "Invalid kernel_entry_stub type: {}",
                other.debug(entry_func.ty, self)
            )),
        };
        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(
                entry_func_return,
                None,
                FunctionControl::NONE,
                entry_func.ty,
            )
            .unwrap();
        let arguments = entry_func_args
            .iter()
            .map(|&ty| emit.function_parameter(ty).unwrap())
            .collect::<Vec<_>>();
        emit.begin_block(None).unwrap();
        let call_result = emit
            .function_call(entry_func_return, None, entry_func.def_cx(self), arguments)
            .unwrap();
        if self.lookup_type(entry_func_return) == SpirvType::Void {
            emit.ret().unwrap();
        } else {
            emit.ret_value(call_result).unwrap();
        }
        emit.end_function().unwrap();

        emit.entry_point(execution_model, fn_id, name, &[]);
        fn_id
    }
}
