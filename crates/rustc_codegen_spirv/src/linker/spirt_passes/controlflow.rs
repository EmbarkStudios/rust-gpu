//! SPIR-T passes related to control-flow.

use crate::custom_insts::{self, CustomOp};
use spirt::{cfg, ControlNodeKind, DataInstKind, DeclDef, ExportKey, Exportee, Module, TypeCtor};

/// Replace our custom extended instruction `Abort`s with standard `OpReturn`s,
/// but only in entry-points (and only before CFG structurization).
pub fn convert_custom_aborts_to_unstructured_returns_in_entry_points(module: &mut Module) {
    let cx = &module.cx();
    let wk = &super::SpvSpecWithExtras::get().well_known;

    let custom_ext_inst_set = cx.intern(&custom_insts::CUSTOM_EXT_INST_SET[..]);

    for (export_key, exportee) in &module.exports {
        let func = match (export_key, exportee) {
            (ExportKey::SpvEntryPoint { .. }, &Exportee::Func(func)) => func,
            _ => continue,
        };

        let func_decl = &mut module.funcs[func];
        assert!(match &cx[func_decl.ret_type].ctor {
            TypeCtor::SpvInst(spv_inst) => spv_inst.opcode == wk.OpTypeVoid,
            _ => false,
        });

        let func_def_body = match &mut func_decl.def {
            DeclDef::Present(def) => def,
            DeclDef::Imported(_) => continue,
        };

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
            if let cfg::ControlInstKind::Unreachable = terminator.kind {
                let abort_inst = block_insts.iter().last.filter(|&last_inst| {
                    match func_def_body.data_insts[last_inst].kind {
                        DataInstKind::SpvExtInst { ext_set, inst } => {
                            ext_set == custom_ext_inst_set
                                && CustomOp::decode(inst) == CustomOp::Abort
                        }
                        _ => false,
                    }
                });
                if let Some(abort_inst) = abort_inst {
                    block_insts.remove(abort_inst, &mut func_def_body.data_insts);
                    terminator.kind = cfg::ControlInstKind::Return;
                }
            }
        }
    }
}
