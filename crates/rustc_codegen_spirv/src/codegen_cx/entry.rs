use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::{AggregatedSpirvAttributes, Entry};
use crate::builder_spirv::SpirvValue;
use crate::builder_spirv::SpirvValueExt;
use crate::codegen_cx::BindlessDescriptorSets;
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::{Decoration, ExecutionModel, FunctionControl, StorageClass, Word};
use rustc_codegen_ssa::traits::BaseTypeMethods;
use rustc_hir as hir;
use rustc_middle::ty::layout::{HasParamEnv, TyAndLayout};
use rustc_middle::ty::{Instance, Ty, TyKind};
use rustc_span::Span;
use rustc_target::abi::{
    call::{ArgAbi, ArgAttribute, ArgAttributes, FnAbi, PassMode},
    Align, LayoutOf, Size,
};
use std::collections::HashMap;

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
        let local_id = match instance.def_id().as_local() {
            Some(id) => id,
            None => {
                self.tcx
                    .sess
                    .err(&format!("Cannot declare {} as an entry point", name));
                return;
            }
        };
        let fn_hir_id = self.tcx.hir().local_def_id_to_hir_id(local_id);
        let body = self.tcx.hir().body(self.tcx.hir().body_owned_by(fn_hir_id));
        const EMPTY: ArgAttribute = ArgAttribute::empty();
        for (abi, arg) in fn_abi.args.iter().zip(body.params) {
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
                    arg.span,
                    &format!("PassMode {:?} invalid for entry point parameter", abi.mode),
                ),
            }
        }
        if let PassMode::Ignore = fn_abi.ret.mode {
        } else {
            self.tcx.sess.span_err(
                self.tcx.hir().span(fn_hir_id),
                &format!(
                    "PassMode {:?} invalid for entry point return type",
                    fn_abi.ret.mode
                ),
            )
        }
        let execution_model = entry.execution_model;
        let fn_id = if execution_model == ExecutionModel::Kernel {
            self.kernel_entry_stub(entry_func, name, execution_model)
        } else {
            self.shader_entry_stub(
                self.tcx.def_span(instance.def_id()),
                entry_func,
                &fn_abi.args,
                body.params,
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

    pub fn bindless_mode(&self) -> bool {
        self.tcx
            .sess
            .contains_name(self.tcx.hir().krate_attrs(), self.sym.spirv_bindless)
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
            is_block: false,
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

        let uniform_uint_ptr =
            SpirvType::Pointer { pointee: uint_ty }.def(rustc_span::DUMMY_SP, self);

        let mut emit_global = self.emit_global();
        let buffer = emit_global
            .variable(uniform_ptr_runtime_array, None, StorageClass::Uniform, None)
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

        emit_global.decorate(
            buffer_struct,
            rspirv::spirv::Decoration::BufferBlock,
            std::iter::empty(),
        );

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
        let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);
        let float_ty = SpirvType::Float(32).def(rustc_span::DUMMY_SP, self);

        let image_2d = SpirvType::Image {
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

        let sampled_image_2d = SpirvType::SampledImage {
            image_type: image_2d,
        }
        .def(rustc_span::DUMMY_SP, self);

        let runtime_array_image = SpirvType::RuntimeArray {
            element: sampled_image_2d,
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
        let void = SpirvType::Void.def(span, self);
        let fn_void_void = SpirvType::Function {
            return_type: void,
            arguments: vec![],
        }
        .def(span, self);
        let entry_func_return_type = match self.lookup_type(entry_func.ty) {
            SpirvType::Function { return_type, .. } => return_type,
            other => self.tcx.sess.fatal(&format!(
                "Invalid entry_stub type: {}",
                other.debug(entry_func.ty, self)
            )),
        };
        let mut decoration_locations = HashMap::new();
        // Create OpVariables before OpFunction so they're global instead of local vars.
        let declared_params = arg_abis
            .iter()
            .zip(hir_params)
            .map(|(entry_fn_arg, hir_param)| {
                self.declare_parameter(entry_fn_arg.layout, hir_param, &mut decoration_locations)
            })
            .collect::<Vec<_>>();
        let len_t = self.type_isize();
        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(void, None, FunctionControl::NONE, fn_void_void)
            .unwrap();
        emit.begin_block(None).unwrap();
        // Adjust any global `OpVariable`s as needed (e.g. loading from `Input`s).
        let arguments: Vec<_> = declared_params
            .iter()
            .zip(arg_abis)
            .zip(hir_params)
            .flat_map(|((&(var, storage_class), entry_fn_arg), hir_param)| {
                let mut dst_len_arg = None;
                let arg = match entry_fn_arg.layout.ty.kind() {
                    TyKind::Ref(_, ty, _) => {
                        if !ty.is_sized(self.tcx.at(span), self.param_env()) {
                            dst_len_arg.replace(
                                self.dst_length_argument(&mut emit, ty, hir_param, len_t, var),
                            );
                        }
                        var
                    }
                    _ => match entry_fn_arg.mode {
                        PassMode::Indirect { .. } => var,
                        PassMode::Direct(_) => {
                            assert_eq!(storage_class, StorageClass::Input);

                            // NOTE(eddyb) this should never fail as it has to have
                            // been already computed earlier by `declare_parameter`.
                            let value_spirv_type =
                                entry_fn_arg.layout.spirv_type(hir_param.span, self);

                            emit.load(value_spirv_type, None, var, None, std::iter::empty())
                                .unwrap()
                        }
                        _ => unreachable!(),
                    },
                };
                std::iter::once(arg).chain(dst_len_arg)
            })
            .collect();
        emit.function_call(
            entry_func_return_type,
            None,
            entry_func.def_cx(self),
            arguments,
        )
        .unwrap();
        emit.ret().unwrap();
        emit.end_function().unwrap();

        let interface: Vec<_> = if emit.version().unwrap() > (1, 3) {
            // SPIR-V >= v1.4 includes all OpVariables in the interface.
            declared_params.into_iter().map(|(var, _)| var).collect()
        } else {
            // SPIR-V <= v1.3 only includes Input and Output in the interface.
            declared_params
                .into_iter()
                .filter(|&(_, s)| s == StorageClass::Input || s == StorageClass::Output)
                .map(|(var, _)| var)
                .collect()
        };
        emit.entry_point(execution_model, fn_id, name, interface);
        fn_id
    }

    fn dst_length_argument(
        &self,
        emit: &mut std::cell::RefMut<'_, rspirv::dr::Builder>,
        ty: Ty<'tcx>,
        hir_param: &hir::Param<'tcx>,
        len_t: Word,
        var: Word,
    ) -> Word {
        match ty.kind() {
            TyKind::Adt(adt_def, substs) => {
                let (member_idx, field_def) = adt_def.all_fields().enumerate().last().unwrap();
                let field_ty = field_def.ty(self.tcx, substs);
                if !matches!(field_ty.kind(), TyKind::Slice(..)) {
                    self.tcx.sess.span_fatal(
                        hir_param.ty_span,
                        "DST parameters are currently restricted to a reference to a struct whose last field is a slice.",
                    )
                }
                emit.array_length(len_t, None, var, member_idx as u32)
                    .unwrap()
            }
            TyKind::Slice(..) | TyKind::Str => self.tcx.sess.span_fatal(
                hir_param.ty_span,
                "Straight slices are not yet supported, wrap the slice in a newtype.",
            ),
            _ => self
                .tcx
                .sess
                .span_fatal(hir_param.ty_span, "Unsupported parameter type."),
        }
    }

    fn declare_parameter(
        &self,
        layout: TyAndLayout<'tcx>,
        hir_param: &hir::Param<'tcx>,
        decoration_locations: &mut HashMap<StorageClass, u32>,
    ) -> (Word, StorageClass) {
        let attrs = AggregatedSpirvAttributes::parse(self, self.tcx.hir().attrs(hir_param.hir_id));

        // FIXME(eddyb) attribute validation should be done ahead of time.
        // FIXME(eddyb) also take into account `&T` interior mutability,
        // i.e. it's only immutable if `T: Freeze`, which we should check.
        // FIXME(eddyb) also check the type for compatibility with being
        // part of the interface, including potentially `Sync`ness etc.
        let (value_ty, storage_class) = if let Some(storage_class_attr) = attrs.storage_class {
            let storage_class = storage_class_attr.value;
            let expected_mutbl = match storage_class {
                StorageClass::UniformConstant
                | StorageClass::Input
                | StorageClass::Uniform
                | StorageClass::PushConstant => hir::Mutability::Not,

                _ => hir::Mutability::Mut,
            };

            match *layout.ty.kind() {
                TyKind::Ref(_, pointee_ty, m) if m == expected_mutbl => (pointee_ty, storage_class),

                _ => self.tcx.sess.span_fatal(
                    hir_param.span,
                    &format!(
                        "invalid entry param type `{}` for storage class `{:?}` \
                         (expected `&{}T`)",
                        layout.ty,
                        storage_class,
                        expected_mutbl.prefix_str()
                    ),
                ),
            }
        } else {
            match *layout.ty.kind() {
                TyKind::Ref(_, pointee_ty, hir::Mutability::Mut) => {
                    (pointee_ty, StorageClass::Output)
                }

                TyKind::Ref(_, pointee_ty, hir::Mutability::Not) => self.tcx.sess.span_fatal(
                    hir_param.span,
                    &format!(
                        "invalid entry param type `{}` (expected `{}` or `&mut {1}`)",
                        layout.ty, pointee_ty
                    ),
                ),

                _ => (layout.ty, StorageClass::Input),
            }
        };

        // Pre-allocate the module-scoped `OpVariable`'s *Result* ID.
        let variable = self.emit_global().id();

        if let hir::PatKind::Binding(_, _, ident, _) = &hir_param.pat.kind {
            self.emit_global().name(variable, ident.to_string());
        }

        let mut decoration_supersedes_location = false;
        if let Some(builtin) = attrs.builtin.map(|attr| attr.value) {
            self.emit_global().decorate(
                variable,
                Decoration::BuiltIn,
                std::iter::once(Operand::BuiltIn(builtin)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(index) = attrs.descriptor_set.map(|attr| attr.value) {
            self.emit_global().decorate(
                variable,
                Decoration::DescriptorSet,
                std::iter::once(Operand::LiteralInt32(index)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(index) = attrs.binding.map(|attr| attr.value) {
            self.emit_global().decorate(
                variable,
                Decoration::Binding,
                std::iter::once(Operand::LiteralInt32(index)),
            );
            decoration_supersedes_location = true;
        }
        if attrs.flat.is_some() {
            self.emit_global()
                .decorate(variable, Decoration::Flat, std::iter::empty());
        }
        if let Some(invariant) = attrs.invariant {
            self.emit_global()
                .decorate(variable, Decoration::Invariant, std::iter::empty());
            if storage_class != StorageClass::Output {
                self.tcx.sess.span_err(
                    invariant.span,
                    "#[spirv(invariant)] is only valid on Output variables",
                );
            }
        }

        // FIXME(eddyb) check whether the storage class is compatible with the
        // specific shader stage of this entry-point, and any decorations
        // (e.g. Vulkan has specific rules for builtin storage classes).

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
                variable,
                Decoration::Location,
                std::iter::once(Operand::LiteralInt32(*location)),
            );
            *location += 1;
        }

        // Emit the `OpVariable` with its *Result* ID set to `variable`.
        let var_spirv_type = SpirvType::Pointer {
            pointee: self.layout_of(value_ty).spirv_type(hir_param.span, self),
        }
        .def(hir_param.span, self);
        self.emit_global()
            .variable(var_spirv_type, Some(variable), storage_class, None);

        (variable, storage_class)
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
