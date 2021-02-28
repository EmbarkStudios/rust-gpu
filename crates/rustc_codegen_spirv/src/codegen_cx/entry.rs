use super::CodegenCx;
use crate::builder_spirv::SpirvValue;
use crate::spirv_type::SpirvType;
use crate::symbols::{parse_attrs, Entry, SpirvAttribute};
use rspirv::dr::Operand;
use rspirv::spirv::{Decoration, ExecutionModel, FunctionControl, StorageClass, Word};
use rustc_hir as hir;
use rustc_middle::ty::{layout::HasParamEnv, Instance, Ty, TyKind};
use rustc_span::Span;
use rustc_target::abi::{
    call::{ArgAbi, ArgAttribute, ArgAttributes, FnAbi, PassMode},
    Size,
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
            if let PassMode::Direct(_) = abi.mode {
            } else if let PassMode::Pair(
                // DST/RTA/VLA
                ArgAttributes {
                    pointee_size: Size::ZERO,
                    ..
                },
                ArgAttributes { regular: EMPTY, .. },
            ) = abi.mode
            {
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
                &fn_abi.args,
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
        hir_params: &[hir::Param<'tcx>],
        arg_abis: &[ArgAbi<'tcx, Ty<'tcx>>],
        name: String,
        execution_model: ExecutionModel,
    ) -> Word {
        let void = SpirvType::Void.def(span, self);
        let fn_void_void = SpirvType::Function {
            return_type: void,
            arguments: vec![],
        }
        .def(span, self);
        let (entry_func_return_type, entry_func_args) = match self.lookup_type(entry_func.ty) {
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
        let new_spirv = self.emit_global().version().unwrap() > (1, 3);
        let arg_len = arg_abis.len();
        let mut arguments = Vec::with_capacity(arg_len);
        let mut interface = if new_spirv {
            Vec::with_capacity(arg_len)
        } else {
            Vec::with_capacity(arg_len / 2)
        };
        let mut rta_lens = Vec::with_capacity(arg_len / 4);
        let mut entry_args = entry_func_args.iter();
        for (hir_param, arg_abi) in hir_params.iter().zip(arg_abis) {
            let arg = *entry_args.next().unwrap_or_else(|| {
                self.tcx.sess.span_fatal(
                    hir_param.span,
                    &format!(
                        "Invalid function arguments: Param {:?} Abi {:?} missing Word",
                        hir_param, arg_abi.layout.ty
                    ),
                )
            });
            let (argument, storage_class) =
                self.declare_parameter(arg, hir_param, arg_abi, &mut decoration_locations);
            if new_spirv || storage_class == StorageClass::Input || storage_class == StorageClass::Output {
                // SPIR-V <= v1.3 only includes Input and Output in the interface.
                interface.push(argument);
            }
            arguments.push(argument);
            if let SpirvType::Pointer { pointee } = self.lookup_type(arg) {
                let spv_ty = self.lookup_type(pointee);
                // TODO check if the RTA elem type is an Image and do not generate a len
                if let SpirvType::RuntimeArray { element } = spv_ty {
                    let len_t = *entry_args.next().unwrap();
                    if let SpirvType::Image {..} = self.lookup_type(element) {
                        arguments.push(u32::MAX);
                        continue;
                    }
                    let struct_rta_t = SpirvType::Adt {
                        def_id: None,
                        align: spv_ty.alignof(self),
                        size: None,
                        field_types: vec![pointee],
                        field_offsets: vec![Size::ZERO],
                        field_names: None,
                        is_block: false,
                    }
                    .def(Span::default(), self);
                    let ptr_struct_rta_t =
                        self.emit_global()
                            .type_pointer(None, storage_class, struct_rta_t);
                    rta_lens.push((arguments.len() as u32, ptr_struct_rta_t, len_t));
                    arguments.push(u32::MAX);
                }
            }
        }
        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(void, None, FunctionControl::NONE, fn_void_void)
            .unwrap();
        emit.begin_block(None).unwrap();
        // add OpArrayLength, make fat pointer struct, pass that into function
        for (len_idx, ptr_struct_rta_t, len_t) in rta_lens {
            let ptr_struct_rta = emit
                .bitcast(ptr_struct_rta_t, None, arguments[(len_idx - 1) as usize])
                .unwrap();
            arguments[len_idx as usize] =
                emit.array_length(len_t, None, ptr_struct_rta, 0).unwrap();
        }
        emit.function_call(entry_func_return_type, None, entry_func.def_cx(self), arguments)
            .unwrap();
        emit.ret().unwrap();
        emit.end_function().unwrap();
        emit.entry_point(execution_model, fn_id, name, interface);
        fn_id
    }

    fn declare_parameter(
        &self,
        arg: Word,
        hir_param: &hir::Param<'tcx>,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        decoration_locations: &mut HashMap<StorageClass, u32>,
    ) -> (Word, StorageClass) {
        let storage_class =
            crate::abi::get_storage_class(self, arg_abi.layout).unwrap_or_else(|| {
                self.tcx.sess.span_fatal(
                    hir_param.span,
                    &format!("invalid entry param type `{}`", arg_abi.layout.ty),
                );
            });
        // Note: this *declares* the variable too.
        let variable = self.emit_global().variable(arg, None, storage_class, None);
        let mut spirv_binding =
            self.parse_storage_class_binding(storage_class, arg_abi.layout.ty.kind());
        if let hir::PatKind::Binding(_, _, ident, _) = &hir_param.pat.kind {
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
                    spirv_binding = SpirvBinding::Builtin;
                }
                SpirvAttribute::Flat => {
                    self.emit_global()
                        .decorate(variable, Decoration::Flat, std::iter::empty());
                }
                _ => {}
            }
        }
        match spirv_binding {
            SpirvBinding::DescriptorSet { set, binding } => {
                self.emit_global().decorate(
                    variable,
                    Decoration::DescriptorSet,
                    std::iter::once(Operand::LiteralInt32(set)),
                );
                self.emit_global().decorate(
                    variable,
                    Decoration::Binding,
                    std::iter::once(Operand::LiteralInt32(binding)),
                );
            }
            SpirvBinding::Location(location) => {
                self.emit_global().decorate(
                    variable,
                    Decoration::Location,
                    std::iter::once(Operand::LiteralInt32(location)),
                );
            }
            SpirvBinding::InferredLocation => {
                // Assign locations from left to right, incrementing each storage class
                // individually.
                // TODO: Is this right for UniformConstant? Do they share locations with
                // input/outpus?
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
            _ => {}
        }
        (variable, storage_class)
    }

    fn parse_storage_class_binding(
        &self,
        storage_class: StorageClass,
        ty_kind: &TyKind<'tcx>,
    ) -> SpirvBinding {
        if let TyKind::Adt(adt, substs) = ty_kind {
            match storage_class {
                StorageClass::Uniform | StorageClass::StorageBuffer => {
                    let desc_set = substs.types().last().unwrap_or_else(|| self.tcx.sess.fatal(&format!(
                        "Uniform or Storage Buffer storage class ADT last type parameter must be binding information. \
                        ADT: {:?} | substs types: {:?}",
                        adt, substs.types().collect::<Vec<_>>()
                    )));
                    if let TyKind::Adt(_, substs2) = desc_set.kind() {
                        let consts = substs2.consts().collect::<Vec<_>>();
                        if consts.len() != 2 {
                            self.tcx.sess.fatal(&format!(
                                "Uniform & Storage Buffer storage class bindings must have set and binding const usize parameters. \
                                desc set: {:?} has const params: {:?}",
                                desc_set, consts
                            ))
                        }
                        let set = consts[0].eval_usize(self.tcx, self.param_env());
                        let binding = consts[1].eval_usize(self.tcx, self.param_env());
                        assert!(set < u32::MAX as u64 && binding < u32::MAX as u64);
                        SpirvBinding::DescriptorSet {
                            set: set as u32,
                            binding: binding as u32,
                        }
                    } else {
                        self.tcx.sess.fatal(&format!(
                            "Uniform or Storage Buffer storage class binding type parameter must be an ADT. \
                            desc set: {:?} is kind: {:?}",
                            desc_set, desc_set.kind()
                    ))
                    }
                }
                StorageClass::Input | StorageClass::Output | StorageClass::UniformConstant => {
                    let location = substs.types().last().unwrap_or_else(|| {
                        self.tcx.sess.fatal(&format!(
                            "Input storage class ADT must have second type parameter for binding. \
                            ADT: {:?} | substs types: {:?}",
                            adt,
                            substs.types().collect::<Vec<_>>()
                        ))
                    });
                    if let TyKind::Adt(_, substs2) = location.kind() {
                        let consts = substs2.consts().collect::<Vec<_>>();
                        if consts.is_empty() {
                            SpirvBinding::InferredLocation
                        } else if consts.len() == 1 {
                            let location = consts[0].eval_usize(self.tcx, self.param_env());
                            assert!(location < u32::MAX as u64);
                            SpirvBinding::Location(location as u32)
                        } else {
                            self.tcx.sess.fatal(&format!(
                                "Input storage class binding type parameter must have zero (inferred/builtin) \
                                or one (explicit) location const parameter. \
                                location: {:?} is kind: {:?}",
                                location, location.kind()
                            ))
                        }
                    } else {
                        self.tcx.sess.fatal(&format!(
                            "Input storage class binding type parameter must be an ADT. \
                            desc set: {:?} is kind: {:?}",
                            location,
                            location.kind()
                        ))
                    }
                }
                StorageClass::PushConstant => SpirvBinding::PushConstant,
                _ => self.tcx.sess.fatal(&format!(
                    "Storage class binding not yet implemented. \
                    storage_class: {:?}",
                    storage_class
                )),
            }
        } else {
            self.tcx.sess.fatal(&format!(
                "Function parameter type kind must be ADT. \
                Type kind: {:?}",
                ty_kind
            ))
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum SpirvBinding {
    DescriptorSet { set: u32, binding: u32 },
    Location(u32),
    InferredLocation,
    Builtin,
    PushConstant,
}
