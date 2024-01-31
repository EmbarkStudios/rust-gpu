//! SPIR-T passes related to control-flow.

use crate::custom_insts::{self, CustomInst, CustomOp};
use smallvec::SmallVec;
use spirt::func_at::FuncAt;
use spirt::{
    cfg, spv, Attr, AttrSet, ConstDef, ConstKind, ControlNodeKind, DataInstFormDef, DataInstKind,
    DeclDef, EntityDefs, ExportKey, Exportee, Module, Type, TypeDef, TypeKind, TypeOrConst, Value,
};
use std::fmt::Write as _;

/// Replace our custom extended instruction `Abort`s with standard `OpReturn`s,
/// but only in entry-points (and only before CFG structurization).
pub fn convert_custom_aborts_to_unstructured_returns_in_entry_points(
    linker_options: &crate::linker::Options,
    module: &mut Module,
) {
    // HACK(eddyb) this shouldn't be the place to parse `abort_strategy`.
    enum Strategy {
        Unreachable,
        DebugPrintf { inputs: bool, backtrace: bool },
    }
    let abort_strategy = linker_options.abort_strategy.as_ref().map(|s| {
        if s == "unreachable" {
            return Strategy::Unreachable;
        }
        if let Some(s) = s.strip_prefix("debug-printf") {
            let (inputs, s) = s.strip_prefix("+inputs").map_or((false, s), |s| (true, s));
            let (backtrace, s) = s
                .strip_prefix("+backtrace")
                .map_or((false, s), |s| (true, s));
            if s.is_empty() {
                return Strategy::DebugPrintf { inputs, backtrace };
            }
        }
        panic!("unknown `--abort-strategy={s}");
    });

    let cx = &module.cx();
    let wk = &super::SpvSpecWithExtras::get().well_known;

    // HACK(eddyb) deduplicate with `diagnostics`.
    let name_from_attrs = |attrs: AttrSet| {
        cx[attrs].attrs.iter().find_map(|attr| match attr {
            Attr::SpvAnnotation(spv_inst) if spv_inst.opcode == wk.OpName => Some(
                super::diagnostics::decode_spv_lit_str_with(&spv_inst.imms, |name| {
                    name.to_string()
                }),
            ),
            _ => None,
        })
    };

    let custom_ext_inst_set = cx.intern(&custom_insts::CUSTOM_EXT_INST_SET[..]);

    for (export_key, exportee) in &module.exports {
        let (entry_point_imms, interface_global_vars, func) = match (export_key, exportee) {
            (
                ExportKey::SpvEntryPoint {
                    imms,
                    interface_global_vars,
                },
                &Exportee::Func(func),
            ) => (imms, interface_global_vars, func),
            _ => continue,
        };

        let func_decl = &mut module.funcs[func];
        assert!(match &cx[func_decl.ret_type].kind {
            TypeKind::SpvInst { spv_inst, .. } => spv_inst.opcode == wk.OpTypeVoid,
            _ => false,
        });

        let func_def_body = match &mut func_decl.def {
            DeclDef::Present(def) => def,
            DeclDef::Imported(_) => continue,
        };

        let debug_printf_context_fmt_str;
        let mut debug_printf_context_inputs = SmallVec::<[_; 4]>::new();
        if let Some(Strategy::DebugPrintf { inputs, .. }) = abort_strategy {
            let mut fmt = String::new();

            match entry_point_imms[..] {
                [spv::Imm::Short(em_kind, _), ref name_imms @ ..] => {
                    assert_eq!(em_kind, wk.ExecutionModel);
                    super::diagnostics::decode_spv_lit_str_with(name_imms, |name| {
                        fmt += &name.replace('%', "%%");
                    });
                }
                _ => unreachable!(),
            }
            fmt += "(";

            // Collect entry-point inputs `OpLoad`ed by the entry block.
            // HACK(eddyb) this relies on Rust-GPU always eagerly loading inputs.
            let loaded_inputs = func_def_body
                .at(func_def_body
                    .at_body()
                    .at_children()
                    .into_iter()
                    .next()
                    .and_then(|func_at_first_node| match func_at_first_node.def().kind {
                        ControlNodeKind::Block { insts } => Some(insts),
                        _ => None,
                    })
                    .unwrap_or_default())
                .into_iter()
                .filter_map(|func_at_inst| {
                    let data_inst_def = func_at_inst.def();
                    let data_inst_form_def = &cx[data_inst_def.form];
                    if let DataInstKind::SpvInst(spv_inst) = &data_inst_form_def.kind {
                        if spv_inst.opcode == wk.OpLoad {
                            if let Value::Const(ct) = data_inst_def.inputs[0] {
                                if let ConstKind::PtrToGlobalVar(gv) = cx[ct].kind {
                                    if interface_global_vars.contains(&gv) {
                                        return Some((
                                            gv,
                                            data_inst_form_def.output_type.unwrap(),
                                            Value::DataInstOutput(func_at_inst.position),
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    None
                });
            if inputs {
                let mut first_input = true;
                for (gv, ty, value) in loaded_inputs {
                    let scalar_type = |ty: Type| match &cx[ty].kind {
                        TypeKind::SpvInst { spv_inst, .. } => match spv_inst.imms[..] {
                            [spv::Imm::Short(_, 32), spv::Imm::Short(_, signedness)]
                                if spv_inst.opcode == wk.OpTypeInt =>
                            {
                                Some(if signedness != 0 { "i" } else { "u" })
                            }
                            [spv::Imm::Short(_, 32)] if spv_inst.opcode == wk.OpTypeFloat => {
                                Some("f")
                            }
                            _ => None,
                        },
                        _ => None,
                    };
                    let vector_or_scalar_type = |ty: Type| {
                        let ty_def = &cx[ty];
                        match &ty_def.kind {
                            TypeKind::SpvInst {
                                spv_inst,
                                type_and_const_inputs,
                            } if spv_inst.opcode == wk.OpTypeVector => {
                                match (&type_and_const_inputs[..], &spv_inst.imms[..]) {
                                    (
                                        &[TypeOrConst::Type(elem)],
                                        &[spv::Imm::Short(_, vlen @ 2..=4)],
                                    ) => Some((scalar_type(elem)?, Some(vlen))),
                                    _ => None,
                                }
                            }
                            _ => Some((scalar_type(ty)?, None)),
                        }
                    };
                    if let Some((scalar_fmt, vlen)) = vector_or_scalar_type(ty) {
                        if !first_input {
                            fmt += ", ";
                        }
                        first_input = false;

                        if let Some(name) = name_from_attrs(module.global_vars[gv].attrs) {
                            fmt += &name.replace('%', "%%");
                            fmt += " = ";
                        }
                        match vlen {
                            Some(vlen) => write!(fmt, "vec{vlen}(%v{vlen}{scalar_fmt})").unwrap(),
                            None => write!(fmt, "%{scalar_fmt}").unwrap(),
                        }
                        debug_printf_context_inputs.push(value);
                    }
                }
            }

            fmt += ")";

            debug_printf_context_fmt_str = fmt;
        } else {
            debug_printf_context_fmt_str = String::new();
        }

        let rpo_regions = func_def_body
            .unstructured_cfg
            .as_ref()
            .expect("Abort->OpReturn can only be done on unstructured CFGs")
            .rev_post_order(func_def_body);
        for region in rpo_regions {
            let region_def = &func_def_body.control_regions[region];
            let control_node_def = match region_def.children.iter().last {
                Some(last_node) => &mut func_def_body.control_nodes[last_node],
                _ => continue,
            };
            let block_insts = match &mut control_node_def.kind {
                ControlNodeKind::Block { insts } => insts,
                _ => continue,
            };

            let terminator = &mut func_def_body
                .unstructured_cfg
                .as_mut()
                .unwrap()
                .control_inst_on_exit_from[region];
            match terminator.kind {
                cfg::ControlInstKind::Unreachable => {}
                _ => continue,
            }

            // HACK(eddyb) this allows accessing the `DataInst` iterator while
            // mutably borrowing other parts of `FuncDefBody`.
            let func_at_block_insts = FuncAt {
                control_nodes: &EntityDefs::new(),
                control_regions: &EntityDefs::new(),
                data_insts: &func_def_body.data_insts,

                position: *block_insts,
            };
            let block_insts_maybe_custom = func_at_block_insts.into_iter().map(|func_at_inst| {
                let data_inst_def = func_at_inst.def();
                (
                    func_at_inst,
                    match cx[data_inst_def.form].kind {
                        DataInstKind::SpvExtInst { ext_set, inst }
                            if ext_set == custom_ext_inst_set =>
                        {
                            Some(CustomOp::decode(inst).with_operands(&data_inst_def.inputs))
                        }
                        _ => None,
                    },
                )
            });
            let custom_terminator_inst = block_insts_maybe_custom
                .clone()
                .rev()
                .take_while(|(_, custom)| custom.is_some())
                .map(|(func_at_inst, custom)| (func_at_inst, custom.unwrap()))
                .find(|(_, custom)| !custom.op().is_debuginfo())
                .filter(|(_, custom)| custom.op().is_terminator());
            if let Some((
                func_at_abort_inst,
                CustomInst::Abort {
                    kind: abort_kind,
                    message_debug_printf,
                },
            )) = custom_terminator_inst
            {
                let abort_inst = func_at_abort_inst.position;
                terminator.kind = cfg::ControlInstKind::Return;

                match abort_strategy {
                    Some(Strategy::Unreachable) => {
                        terminator.kind = cfg::ControlInstKind::Unreachable;
                    }
                    Some(Strategy::DebugPrintf {
                        inputs: _,
                        backtrace,
                    }) => {
                        let const_kind = |v: Value| match v {
                            Value::Const(ct) => &cx[ct].kind,
                            _ => unreachable!(),
                        };
                        let const_str = |v: Value| match const_kind(v) {
                            &ConstKind::SpvStringLiteralForExtInst(s) => s,
                            _ => unreachable!(),
                        };
                        let const_u32 = |v: Value| match const_kind(v) {
                            ConstKind::SpvInst {
                                spv_inst_and_const_inputs,
                            } => {
                                let (spv_inst, _const_inputs) = &**spv_inst_and_const_inputs;
                                assert!(spv_inst.opcode == wk.OpConstant);
                                match spv_inst.imms[..] {
                                    [spv::Imm::Short(_, x)] => x,
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        };
                        let mk_const_str = |s| {
                            cx.intern(ConstDef {
                                attrs: Default::default(),
                                ty: cx.intern(TypeDef {
                                    attrs: Default::default(),
                                    kind: TypeKind::SpvStringLiteralForExtInst,
                                }),
                                kind: ConstKind::SpvStringLiteralForExtInst(s),
                            })
                        };

                        let mut current_debug_src_loc = None;
                        let mut call_stack = SmallVec::<[_; 8]>::new();
                        let block_insts_custom = block_insts_maybe_custom
                            .filter_map(|(func_at_inst, custom)| Some((func_at_inst, custom?)));
                        for (func_at_inst, custom) in block_insts_custom {
                            // Stop at the abort, that we don't undo its debug context.
                            if func_at_inst.position == abort_inst {
                                break;
                            }

                            match custom {
                                CustomInst::SetDebugSrcLoc {
                                    file,
                                    line_start,
                                    line_end: _,
                                    col_start,
                                    col_end: _,
                                } => {
                                    current_debug_src_loc = Some((
                                        &cx[const_str(file)],
                                        const_u32(line_start),
                                        const_u32(col_start),
                                    ));
                                }
                                CustomInst::ClearDebugSrcLoc => current_debug_src_loc = None,
                                CustomInst::PushInlinedCallFrame { callee_name } => {
                                    if backtrace {
                                        call_stack.push((
                                            current_debug_src_loc.take(),
                                            const_str(callee_name),
                                        ));
                                    }
                                }
                                CustomInst::PopInlinedCallFrame => {
                                    if let Some((callsite_debug_src_loc, _)) = call_stack.pop() {
                                        current_debug_src_loc = callsite_debug_src_loc;
                                    }
                                }
                                CustomInst::Abort { .. } => {}
                            }
                        }

                        let mut fmt = String::new();

                        let (message_debug_printf_fmt_str, message_debug_printf_args) =
                            message_debug_printf
                                .split_first()
                                .map(|(&fmt_str, args)| (&cx[const_str(fmt_str)], args))
                                .unwrap_or_default();

                        let fmt_dbg_src_loc = |(file, line, col)| {
                            // FIXME(eddyb) figure out what is going on with
                            // these column number conventions, below is a
                            // related comment from `spirt::print`:
                            // > // HACK(eddyb) Rust-GPU's column numbers seem
                            // > // off-by-one wrt what e.g. VSCode expects
                            // > // for `:line:col` syntax, but it's hard to
                            // > // tell from the spec and `glslang` doesn't
                            // > // even emit column numbers at all!
                            let col = col + 1;
                            format!("{file}:{line}:{col}").replace('%', "%%")
                        };

                        // HACK(eddyb) this improves readability w/ very verbose Vulkan loggers.
                        fmt += "\n";

                        fmt += "[Rust ";

                        // HACK(eddyb) turn "panic" into "panicked", while the
                        // general case looks like "abort" -> "aborted".
                        match &cx[const_str(abort_kind)] {
                            "panic" => fmt += "panicked",
                            verb => {
                                fmt += verb;
                                fmt += "en";
                            }
                        };

                        if let Some(loc) = current_debug_src_loc.take() {
                            fmt += " at ";
                            fmt += &fmt_dbg_src_loc(loc);
                        }

                        fmt += "]\n ";
                        fmt += &message_debug_printf_fmt_str.replace('\n', "\n ");

                        let mut innermost = true;
                        let mut append_call = |callsite_debug_src_loc, callee: &str| {
                            if innermost {
                                innermost = false;
                                fmt += "\n      in ";
                            } else if current_debug_src_loc.is_some() {
                                fmt += "\n      by ";
                            } else {
                                // HACK(eddyb) previous call didn't have a `called at` line.
                                fmt += "\n      called by ";
                            }
                            fmt += callee;
                            if let Some(loc) = callsite_debug_src_loc {
                                fmt += "\n        called at ";
                                fmt += &fmt_dbg_src_loc(loc);
                            }
                            current_debug_src_loc = callsite_debug_src_loc;
                        };
                        while let Some((callsite_debug_src_loc, callee)) = call_stack.pop() {
                            append_call(callsite_debug_src_loc, &cx[callee].replace('%', "%%"));
                        }
                        append_call(None, &debug_printf_context_fmt_str);

                        fmt += "\n";

                        let abort_inst_def = &mut func_def_body.data_insts[abort_inst];
                        abort_inst_def.form = cx.intern(DataInstFormDef {
                            kind: DataInstKind::SpvExtInst {
                                ext_set: cx.intern("NonSemantic.DebugPrintf"),
                                inst: 1,
                            },
                            output_type: cx[abort_inst_def.form].output_type,
                        });
                        abort_inst_def.inputs = [Value::Const(mk_const_str(cx.intern(fmt)))]
                            .into_iter()
                            .chain(message_debug_printf_args.iter().copied())
                            .chain(debug_printf_context_inputs.iter().copied())
                            .collect();

                        // Avoid removing the instruction we just replaced.
                        continue;
                    }
                    None => {}
                }
                block_insts.remove(abort_inst, &mut func_def_body.data_insts);
            }
        }
    }
}
