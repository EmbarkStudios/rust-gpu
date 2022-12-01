use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::{AggregatedSpirvAttributes, Entry, Spanned};
use crate::builder::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::{
    Capability, Decoration, Dim, ExecutionModel, FunctionControl, StorageClass, Word,
};
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir as hir;
use rustc_middle::span_bug;
use rustc_middle::ty::layout::{LayoutOf, TyAndLayout};
use rustc_middle::ty::{self, Instance, Ty};
use rustc_span::Span;
use rustc_target::abi::call::{ArgAbi, FnAbi, PassMode};
use std::assert_matches::assert_matches;

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
            let fn_local_def_id = if let Some(id) = instance.def_id().as_local() {
                id
            } else {
                self.tcx
                    .sess
                    .span_err(span, format!("Cannot declare {} as an entry point", name));
                return;
            };
            let body = self
                .tcx
                .hir()
                .body(self.tcx.hir().body_owned_by(fn_local_def_id));
            body.params
        };
        for (arg_abi, hir_param) in fn_abi.args.iter().zip(hir_params) {
            match arg_abi.mode {
                PassMode::Direct(_) => {}
                PassMode::Pair(..) => {
                    // FIXME(eddyb) implement `ScalarPair` `Input`s, or change
                    // the `FnAbi` readjustment to only use `PassMode::Pair` for
                    // pointers to `!Sized` types, but not other `ScalarPair`s.
                    if !matches!(arg_abi.layout.ty.kind(), ty::Ref(..)) {
                        self.tcx.sess.span_err(
                            hir_param.ty_span,
                            format!(
                                "entry point parameter type not yet supported \
                                 (`{}` has `ScalarPair` ABI but is not a `&T`)",
                                arg_abi.layout.ty
                            ),
                        );
                    }
                }
                // FIXME(eddyb) support these (by just ignoring them) - if there
                // is any validation concern, it should be done on the types.
                PassMode::Ignore => self.tcx.sess.span_fatal(
                    hir_param.ty_span,
                    format!(
                        "entry point parameter type not yet supported \
                        (`{}` has size `0`)",
                        arg_abi.layout.ty
                    ),
                ),
                _ => span_bug!(
                    hir_param.ty_span,
                    "query hooks should've made this `PassMode` impossible: {:#?}",
                    arg_abi
                ),
            }
        }
        if fn_abi.ret.layout.ty.is_unit() {
            assert_matches!(fn_abi.ret.mode, PassMode::Ignore);
        } else {
            self.tcx.sess.span_err(
                span,
                format!(
                    "entry point should return `()`, not `{}`",
                    fn_abi.ret.layout.ty
                ),
            );
        }

        // let execution_model = entry.execution_model;
        let fn_id = self.shader_entry_stub(
            span,
            entry_func,
            fn_abi,
            hir_params,
            name,
            entry.execution_model,
        );
        let mut emit = self.emit_global();
        entry
            .execution_modes
            .iter()
            .for_each(|(execution_mode, execution_mode_extra)| {
                emit.execution_mode(fn_id, *execution_mode, execution_mode_extra);
            });
    }

    fn shader_entry_stub(
        &self,
        span: Span,
        entry_func: SpirvValue,
        entry_fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        hir_params: &[hir::Param<'tcx>],
        name: String,
        execution_model: ExecutionModel,
    ) -> Word {
        let stub_fn = {
            let void = SpirvType::Void.def(span, self);
            let fn_void_void = SpirvType::Function {
                return_type: void,
                arguments: &[],
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
        for (entry_arg_abi, hir_param) in entry_fn_abi.args.iter().zip(hir_params) {
            bx.set_span(hir_param.span);
            self.declare_shader_interface_for_param(
                execution_model,
                entry_arg_abi,
                hir_param,
                &mut op_entry_point_interface_operands,
                &mut bx,
                &mut call_args,
                &mut decoration_locations,
            );
        }
        bx.set_span(span);
        bx.call(
            entry_func.ty,
            Some(entry_fn_abi),
            entry_func,
            &call_args,
            None,
        );
        bx.ret_void();

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
            ty::Ref(_, pointee_ty, mutbl) => (pointee_ty, mutbl, true),
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
                        format!(
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
                    format!(
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
                Some(inferred) => {
                    self.tcx
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
                        .emit();
                }
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
                    format!(
                        "invalid entry param type `{}` (expected `{}` or `&mut {1}`)",
                        layout.ty, value_ty
                    ),
                ),
            });

        (spirv_ty, storage_class)
    }

    #[allow(clippy::too_many_arguments)]
    fn declare_shader_interface_for_param(
        &self,
        execution_model: ExecutionModel,
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
                let var_spirv_type = SpirvType::InterfaceBlock {
                    inner_type: value_spirv_type,
                }
                .def(hir_param.span, self);
                var_ptr_spirv_type = self.type_ptr_to(var_spirv_type);

                let value_ptr = bx.struct_gep(var_spirv_type, var.with_type(var_ptr_spirv_type), 0);

                let value_len = if is_unsized_with_len {
                    match self.lookup_type(value_spirv_type) {
                        SpirvType::RuntimeArray { .. } => {}
                        _ => {
                            self.tcx.sess.span_err(
                                hir_param.ty_span,
                                "only plain slices are supported as unsized types",
                            );
                        }
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
                        format!(
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
        if let ty::Ref(..) = entry_arg_abi.layout.ty.kind() {
            call_args.push(value_ptr);
            match entry_arg_abi.mode {
                PassMode::Direct(_) => assert_eq!(value_len, None),
                PassMode::Pair(..) => call_args.push(value_len.unwrap()),
                _ => unreachable!(),
            }
        } else {
            assert_eq!(storage_class, StorageClass::Input);
            assert_matches!(entry_arg_abi.mode, PassMode::Direct(_));

            let value = bx.load(
                entry_arg_abi.layout.spirv_type(hir_param.ty_span, bx),
                value_ptr,
                entry_arg_abi.layout.align.abi,
            );
            call_args.push(value);
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
            self.emit_global().decorate(
                var,
                Decoration::DescriptorSet,
                std::iter::once(Operand::LiteralInt32(index)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(index) = attrs.binding.map(|attr| attr.value) {
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

        self.check_for_bad_types(
            execution_model,
            hir_param.ty_span,
            var_ptr_spirv_type,
            storage_class,
            attrs.builtin.is_some(),
            attrs.flat,
        );

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

    // Booleans are only allowed in some storage classes. Error if they're in others.
    // Integers and `f64`s must be decorated with `#[spirv(flat)]`.
    fn check_for_bad_types(
        &self,
        execution_model: ExecutionModel,
        span: Span,
        ty: Word,
        storage_class: StorageClass,
        is_builtin: bool,
        flat_attr: Option<Spanned<()>>,
    ) {
        // private and function are allowed here, but they can't happen.
        if matches!(
            storage_class,
            StorageClass::Workgroup | StorageClass::CrossWorkgroup
        ) {
            return;
        }

        let mut has_bool = false;
        let mut type_must_be_flat = false;
        recurse(self, ty, &mut has_bool, &mut type_must_be_flat);

        // SPIR-V technically allows all input/output variables to be booleans, not just builtins,
        // but has a note:
        // > Khronos Issue #363: OpTypeBool can be used in the Input and Output storage classes,
        //   but the client APIs still only allow built-in Boolean variables (e.g. FrontFacing),
        //   not user variables.
        // spirv-val disallows non-builtin inputs/outputs, so we do too, I guess.
        if has_bool
            && !(is_builtin && matches!(storage_class, StorageClass::Input | StorageClass::Output))
        {
            self.tcx
                .sess
                .span_err(span, "entry-point parameter cannot contain `bool`s");
        }

        // Enforce Vulkan validation rules around `Flat` as accurately as possible,
        // i.e. "interpolation control" can only be used "within" the rasterization
        // pipeline (roughly: `vertex (outputs) -> ... -> (inputs for) fragment`),
        // but not at the "outer" interface (vertex inputs/fragment outputs).
        // Also, fragment inputs *require* it for some ("uninterpolatable") types.
        // FIXME(eddyb) maybe this kind of `enum` could be placed elsewhere?
        enum Force {
            Disallow,
            Require,
        }
        #[allow(clippy::match_same_arms)]
        let flat_forced = match (execution_model, storage_class) {
            // VUID-StandaloneSpirv-Flat-06202
            // > The `Flat`, `NoPerspective`, `Sample`, and `Centroid` decorations **must**
            // > not be used on variables with the `Input` storage class in a vertex shader
            (ExecutionModel::Vertex, StorageClass::Input) => Some(Force::Disallow),

            // VUID-StandaloneSpirv-Flat-04744
            // > Any variable with integer or double-precision floating-point type and
            // > with `Input` storage class in a fragment shader, **must** be decorated `Flat`
            (ExecutionModel::Fragment, StorageClass::Input) if type_must_be_flat => {
                // FIXME(eddyb) shouldn't this be automatic then? (maybe with a warning?)
                Some(Force::Require)
            }

            // VUID-StandaloneSpirv-Flat-06201
            // > The `Flat`, `NoPerspective`, `Sample`, and `Centroid` decorations **must**
            // > not be used on variables with the `Output` storage class in a fragment shader
            (ExecutionModel::Fragment, StorageClass::Output) => Some(Force::Disallow),

            // VUID-StandaloneSpirv-Flat-04670
            // > The `Flat`, `NoPerspective`, `Sample`, and `Centroid` decorations **must**
            // > only be used on variables with the `Output` or `Input` storage class
            (_, StorageClass::Input | StorageClass::Output) => None,
            _ => Some(Force::Disallow),
        };

        let flat_mismatch = match (flat_forced, flat_attr) {
            (Some(Force::Disallow), Some(flat_attr)) => Some((flat_attr.span, "cannot")),
            // FIXME(eddyb) it would be useful to show the type that required it.
            (Some(Force::Require), None) => Some((span, "must")),
            _ => None,
        };
        if let Some((span, must_or_cannot)) = flat_mismatch {
            self.tcx.sess.span_err(
                span,
                format!(
                    "`{execution_model:?}` entry-point `{storage_class:?}` parameter \
                     {must_or_cannot} be decorated with `#[spirv(flat)]`"
                ),
            );
        }

        fn recurse(cx: &CodegenCx<'_>, ty: Word, has_bool: &mut bool, must_be_flat: &mut bool) {
            match cx.lookup_type(ty) {
                SpirvType::Bool => *has_bool = true,
                SpirvType::Integer(_, _) | SpirvType::Float(64) => *must_be_flat = true,
                SpirvType::Adt { field_types, .. } => {
                    for &f in field_types {
                        recurse(cx, f, has_bool, must_be_flat);
                    }
                }
                SpirvType::Vector { element, .. }
                | SpirvType::Matrix { element, .. }
                | SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element }
                | SpirvType::Pointer { pointee: element }
                | SpirvType::InterfaceBlock {
                    inner_type: element,
                } => recurse(cx, element, has_bool, must_be_flat),
                SpirvType::Function {
                    return_type,
                    arguments,
                } => {
                    recurse(cx, return_type, has_bool, must_be_flat);
                    for &a in arguments {
                        recurse(cx, a, has_bool, must_be_flat);
                    }
                }
                _ => (),
            }
        }
    }
}
