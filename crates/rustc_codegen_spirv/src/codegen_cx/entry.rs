use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::{AggregatedSpirvAttributes, Entry};
use crate::builder_spirv::SpirvValue;
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::{Decoration, ExecutionModel, FunctionControl, StorageClass, Word};
use rustc_hir as hir;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{Instance, Ty, TyKind};
use rustc_span::Span;
use rustc_target::abi::call::{FnAbi, PassMode};
use rustc_target::abi::LayoutOf;
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
        for (abi, arg) in fn_abi.args.iter().zip(body.params) {
            match abi.mode {
                PassMode::Direct(_) | PassMode::Indirect { .. } => {}
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
                fn_abi,
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

    fn shader_entry_stub(
        &self,
        span: Span,
        entry_func: SpirvValue,
        entry_fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
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
            SpirvType::Function {
                return_type,
                arguments: _,
            } => return_type,
            other => self.tcx.sess.fatal(&format!(
                "Invalid entry_stub type: {}",
                other.debug(entry_func.ty, self)
            )),
        };
        let mut decoration_locations = HashMap::new();
        // Create OpVariables before OpFunction so they're global instead of local vars.
        let declared_params = entry_fn_abi
            .args
            .iter()
            .zip(hir_params)
            .map(|(entry_fn_arg, hir_param)| {
                self.declare_parameter(entry_fn_arg.layout, hir_param, &mut decoration_locations)
            })
            .collect::<Vec<_>>();
        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(void, None, FunctionControl::NONE, fn_void_void)
            .unwrap();
        emit.begin_block(None).unwrap();
        // Adjust any global `OpVariable`s as needed (e.g. loading from `Input`s).
        let arguments: Vec<_> = declared_params
            .iter()
            .zip(&entry_fn_abi.args)
            .zip(hir_params)
            .map(|((&(var, storage_class), entry_fn_arg), hir_param)| {
                match entry_fn_arg.layout.ty.kind() {
                    TyKind::Ref(..) => var,

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
                }
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
