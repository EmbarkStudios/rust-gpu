use rspirv::dr::{Instruction, Module};
use rspirv::spirv::Word;
use std::collections::HashSet;

pub fn dce(module: &mut Module) {
    let mut rooted = collect_roots(module);
    while spread_roots(module, &mut rooted) {}
    kill_unrooted(module, &rooted);
}

pub fn collect_roots(module: &Module) -> HashSet<Word> {
    let mut rooted = HashSet::new();
    for inst in &module.entry_points {
        root(inst, &mut rooted);
    }
    rooted
}

fn spread_roots(module: &Module, rooted: &mut HashSet<Word>) -> bool {
    let mut any = false;
    for inst in module.global_inst_iter() {
        if let Some(id) = inst.result_id {
            if rooted.contains(&id) {
                any |= root(inst, rooted);
            }
        }
    }
    for func in &module.functions {
        if rooted.contains(&func.def.as_ref().unwrap().result_id.unwrap()) {
            for inst in &func.def {
                any |= root(inst, rooted);
            }
            for inst in &func.parameters {
                any |= root(inst, rooted);
            }
            for block in &func.blocks {
                for inst in &block.instructions {
                    any |= root(inst, rooted);
                }
            }
        }
    }
    any
}

fn root(inst: &Instruction, rooted: &mut HashSet<Word>) -> bool {
    let mut any = false;
    if let Some(id) = inst.result_type {
        any |= rooted.insert(id);
    }
    for op in &inst.operands {
        if let Some(id) = op.id_ref_any() {
            any |= rooted.insert(id);
        }
    }
    any
}

fn is_rooted(inst: &Instruction, rooted: &HashSet<Word>) -> bool {
    if let Some(result_id) = inst.result_id {
        rooted.contains(&result_id)
    } else {
        // For things like OpDecorate which apply attributes to rooted things, but are not
        // referenced by roots
        inst.operands
            .iter()
            .any(|op| op.id_ref_any().map_or(false, |w| rooted.contains(&w)))
    }
}

fn kill_unrooted(module: &mut Module, rooted: &HashSet<Word>) {
    module
        .ext_inst_imports
        .retain(|inst| is_rooted(inst, rooted));
    module.debugs.retain(|inst| is_rooted(inst, rooted));
    module.annotations.retain(|inst| is_rooted(inst, rooted));
    module
        .types_global_values
        .retain(|inst| is_rooted(inst, rooted));
    module
        .functions
        .retain(|f| is_rooted(f.def.as_ref().unwrap(), rooted));
}
