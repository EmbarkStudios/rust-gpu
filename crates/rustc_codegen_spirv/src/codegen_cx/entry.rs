use super::CodegenCx;
use crate::builder_spirv::SpirvValue;
use crate::spirv_type::SpirvType;
use crate::symbols::{parse_attrs, Entry, SpirvAttribute};
use rspirv::dr::Operand;
use rspirv::spirv::{Decoration, ExecutionModel, FunctionControl, StorageClass, Word};
use rustc_hir::{Param, PatKind};
use rustc_middle::ty::{Instance, Ty};
use rustc_span::Span;
use rustc_target::abi::call::{FnAbi, PassMode};
use std::collections::HashMap;

impl<'tcx> CodegenCx<'tcx> {
    // Entry points declare their "interface" (all uniforms, inputs, outputs, etc.) as parameters.
    // spir-v uses globals to declare the interface. So, we need to generate a lil stub for the
    // "real" main that collects all those global variables and calls the user-defined main
    // function.
    pub fn entry_stub(
        &self,
        instance: &Instance<'_>,
        fn_abi: &FnAbi<'_, Ty<'_>>,
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
            if let PassMode::Direct(_) = abi.mode {
            } else {
                self.tcx.sess.span_err(
                    arg.span,
                    &format!("PassMode {:?} invalid for entry point parameter", abi.mode),
                )
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
        hir_params: &[Param<'tcx>],
        name: String,
        execution_model: ExecutionModel,
    ) -> Word {
        let void = SpirvType::Void.def(span, self);
        let fn_void_void = SpirvType::Function {
            return_type: void,
            arguments: vec![],
        }
        .def(span, self);
        let (entry_func_return, entry_func_args) = match self.lookup_type(entry_func.ty) {
            SpirvType::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => self.tcx.sess.fatal(&format!(
                "Invalid entry_stub type: {}",
                other.debug(entry_func.ty, self)
            )),
        };
        let mut decoration_locations = HashMap::new();
        // Create OpVariables before OpFunction so they're global instead of local vars.
        let arguments = entry_func_args
            .iter()
            .zip(hir_params)
            .map(|(&arg, hir_param)| {
                self.declare_parameter(arg, hir_param, &mut decoration_locations)
            })
            .collect::<Vec<_>>();
        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(void, None, FunctionControl::NONE, fn_void_void)
            .unwrap();
        emit.begin_block(None).unwrap();
        emit.function_call(
            entry_func_return,
            None,
            entry_func.def_cx(self),
            arguments.iter().map(|&(a, _)| a),
        )
        .unwrap();
        emit.ret().unwrap();
        emit.end_function().unwrap();

        let interface: Vec<_> = if emit.version().unwrap() > (1, 3) {
            // SPIR-V >= v1.4 includes all OpVariables in the interface.
            arguments.into_iter().map(|(a, _)| a).collect()
        } else {
            // SPIR-V <= v1.3 only includes Input and Output in the interface.
            arguments
                .into_iter()
                .filter(|&(_, s)| s == StorageClass::Input || s == StorageClass::Output)
                .map(|(a, _)| a)
                .collect()
        };
        emit.entry_point(execution_model, fn_id, name, interface);
        fn_id
    }

    fn declare_parameter(
        &self,
        arg: Word,
        hir_param: &Param<'tcx>,
        decoration_locations: &mut HashMap<StorageClass, u32>,
    ) -> (Word, StorageClass) {
        let storage_class = match self.lookup_type(arg) {
            SpirvType::Pointer { storage_class, .. } => storage_class,
            other => self.tcx.sess.fatal(&format!(
                "Invalid entry arg type {}",
                other.debug(arg, self)
            )),
        };

        // Handles fn main(#[spirv(input)] x: &f32) // etc
        // FIXME: This could be cleaned up and merged with the location logic
        let mut parameter_storage_class = None;
        for attr in parse_attrs(self, hir_param.attrs) {
            match attr {
                SpirvAttribute::StorageClass(s) => {
                    if parameter_storage_class.is_none() {
                        parameter_storage_class.replace(s);
                    } else {
                        // FIXME: Add span
                        self.tcx
                            .sess
                            .fatal("Multiple storage class attributes for entry parameter!")
                    }
                }
                _ => (),
            }
        }
        // Only the new way as above or the old way via Input<T> are allowed, not both
        let storage_class = match (storage_class, parameter_storage_class) {
            (StorageClass::Function, Some(s)) => s,
            (s, None) => s,
            (_, Some(_)) => {
                // FIXME: Add span
                self.tcx
                    .sess
                    .fatal("Storage class specified with both type and on entry parameter!")
            }
        };

        let mut has_location = matches!(
            storage_class,
            StorageClass::Input | StorageClass::Output | StorageClass::UniformConstant
        );
        // Note: this *declares* the variable too
        let variable = self.emit_global().variable(arg, None, storage_class, None);
        if let PatKind::Binding(_, _, ident, _) = &hir_param.pat.kind {
            self.emit_global().name(variable, ident.to_string());
        }
        for attr in parse_attrs(self, hir_param.attrs) {
            match attr {
                SpirvAttribute::Builtin(builtin) => {
                    self.emit_global().decorate(
                        variable,
                        Decoration::BuiltIn,
                        std::iter::once(Operand::BuiltIn(builtin)),
                    );
                    has_location = false;
                }
                SpirvAttribute::DescriptorSet(index) => {
                    self.emit_global().decorate(
                        variable,
                        Decoration::DescriptorSet,
                        std::iter::once(Operand::LiteralInt32(index)),
                    );
                    has_location = false;
                }
                SpirvAttribute::Binding(index) => {
                    self.emit_global().decorate(
                        variable,
                        Decoration::Binding,
                        std::iter::once(Operand::LiteralInt32(index)),
                    );
                    has_location = false;
                }
                _ => {}
            }
        }
        // Assign locations from left to right, incrementing each storage class
        // individually.
        // TODO: Is this right for UniformConstant? Do they share locations with
        // input/outpus?
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
