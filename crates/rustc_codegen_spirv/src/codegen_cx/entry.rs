use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::{AggregatedSpirvAttributes, Entry};
use crate::builder::Builder;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::{Decoration, ExecutionModel, FunctionControl, StorageClass, Word};
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods};
use rustc_hir as hir;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{Instance, Ty, TyKind};
use rustc_span::Span;
use rustc_target::abi::{
    call::{ArgAbi, ArgAttribute, ArgAttributes, FnAbi, PassMode},
    LayoutOf, Size,
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
            )
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
        let mut decoration_locations = HashMap::new();
        let interface_globals = arg_abis
            .iter()
            .zip(hir_params)
            .map(|(entry_fn_arg, hir_param)| {
                self.declare_shader_interface_for_param(
                    entry_fn_arg.layout,
                    hir_param,
                    &mut op_entry_point_interface_operands,
                    &mut decoration_locations,
                )
            })
            .collect::<Vec<_>>();
        let mut bx = Builder::new_block(self, stub_fn, "");
        // Adjust any global `OpVariable`s as needed (e.g. loading from `Input`s,
        // or accessing the sole field of an "interface block" `OpTypeStruct`),
        // to match the argument type we have to pass to the Rust entry `fn`.
        let arguments: Vec<_> = interface_globals
            .iter()
            .zip(arg_abis)
            .zip(hir_params)
            .flat_map(
                |((&(global_var, storage_class), entry_fn_arg), hir_param)| {
                    bx.set_span(hir_param.span);

                    let var_value_spirv_type = match self.lookup_type(global_var.ty) {
                        SpirvType::Pointer { pointee } => pointee,
                        _ => unreachable!(),
                    };

                    let (first, second) = match entry_fn_arg.layout.ty.kind() {
                        TyKind::Ref(_, pointee_ty, _) => {
                            let arg_pointee_spirv_type = self
                                .layout_of(pointee_ty)
                                .spirv_type(hir_param.ty_span, self);

                            if let SpirvType::InterfaceBlock { inner_type } =
                                self.lookup_type(var_value_spirv_type)
                            {
                                assert_ty_eq!(self, arg_pointee_spirv_type, inner_type);

                                let inner = bx.struct_gep(global_var, 0);

                                match entry_fn_arg.mode {
                                    PassMode::Direct(_) => (inner, None),

                                    // Unsized pointee with length (i.e. `&[T]`).
                                    PassMode::Pair(..) => {
                                        // FIXME(eddyb) shouldn't this be `usize`?
                                        let len_spirv_type = self.type_isize();

                                        let len = bx
                                            .emit()
                                            .array_length(
                                                len_spirv_type,
                                                None,
                                                global_var.def(&bx),
                                                0,
                                            )
                                            .unwrap()
                                            .with_type(len_spirv_type);

                                        (inner, Some(len))
                                    }

                                    _ => unreachable!(),
                                }
                            } else {
                                assert_ty_eq!(self, arg_pointee_spirv_type, var_value_spirv_type);
                                assert_matches!(entry_fn_arg.mode, PassMode::Direct(_));
                                (global_var, None)
                            }
                        }
                        _ => {
                            assert_eq!(storage_class, StorageClass::Input);

                            let arg_spirv_type =
                                entry_fn_arg.layout.spirv_type(hir_param.ty_span, self);

                            assert_ty_eq!(self, arg_spirv_type, var_value_spirv_type);

                            match entry_fn_arg.mode {
                                PassMode::Indirect { .. } => (global_var, None),
                                PassMode::Direct(_) => {
                                    (bx.load(global_var, entry_fn_arg.layout.align.abi), None)
                                }
                                _ => unreachable!(),
                            }
                        }
                    };
                    std::iter::once(first).chain(second)
                },
            )
            .collect();
        bx.set_span(span);
        bx.call(entry_func, &arguments, None);
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
            TyKind::Ref(_, pointee_ty, mutbl) => (pointee_ty, mutbl, true),
            _ => (layout.ty, hir::Mutability::Not, false),
        };
        let spirv_ty = self.layout_of(value_ty).spirv_type(hir_param.ty_span, self);
        // Some types automatically specify a storage class. Compute that here.
        let inferred_storage_class_from_ty = match self.lookup_type(spirv_ty) {
            SpirvType::Image { .. } | SpirvType::Sampler | SpirvType::SampledImage { .. } => {
                Some(StorageClass::UniformConstant)
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
        layout: TyAndLayout<'tcx>,
        hir_param: &hir::Param<'tcx>,
        op_entry_point_interface_operands: &mut Vec<Word>,
        decoration_locations: &mut HashMap<StorageClass, u32>,
    ) -> (SpirvValue, StorageClass) {
        let attrs = AggregatedSpirvAttributes::parse(self, self.tcx.hir().attrs(hir_param.hir_id));

        // Pre-allocate the module-scoped `OpVariable`'s *Result* ID.
        let var = self.emit_global().id();

        let (mut value_spirv_type, storage_class) =
            self.infer_param_ty_and_storage_class(layout, hir_param, &attrs);

        // Certain storage classes require an `OpTypeStruct` decorated with `Block`,
        // which we represent with `SpirvType::InterfaceBlock` (see its doc comment).
        // This "interface block" construct is also required for "runtime arrays".
        let is_unsized = self.lookup_type(value_spirv_type).sizeof(self).is_none();
        match storage_class {
            StorageClass::PushConstant | StorageClass::Uniform | StorageClass::StorageBuffer => {
                if is_unsized {
                    match self.lookup_type(value_spirv_type) {
                        SpirvType::RuntimeArray { .. } => {}
                        _ => self.tcx.sess.span_err(
                            hir_param.ty_span,
                            "only plain slices are supported as unsized types",
                        ),
                    }
                }

                value_spirv_type = SpirvType::InterfaceBlock {
                    inner_type: value_spirv_type,
                }
                .def(hir_param.span, self);
            }
            _ => {
                if is_unsized {
                    self.tcx.sess.span_fatal(
                        hir_param.ty_span,
                        &format!(
                            "unsized types are not supported for storage class {:?}",
                            storage_class
                        ),
                    );
                }
            }
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
        let var_spirv_type = SpirvType::Pointer {
            pointee: value_spirv_type,
        }
        .def(hir_param.span, self);
        self.emit_global()
            .variable(var_spirv_type, Some(var), storage_class, None);

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

        (var.with_type(var_spirv_type), storage_class)
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
