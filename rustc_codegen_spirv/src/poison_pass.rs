use rspirv::dr::{Instruction, Module, Operand};
use rspirv::spirv::{Op, Word};
use std::collections::HashSet;

fn contains_poison(inst: &Instruction, poison: &HashSet<Word>) -> bool {
    inst.result_type.map_or(false, |w| poison.contains(&w))
        || inst.operands.iter().any(|op| match op {
            rspirv::dr::Operand::IdMemorySemantics(w)
            | rspirv::dr::Operand::IdScope(w)
            | rspirv::dr::Operand::IdRef(w) => poison.contains(w),
            _ => false,
        })
}

fn is_poison(inst: &Instruction, poison: &HashSet<Word>) -> bool {
    if let Some(result_id) = inst.result_id {
        poison.contains(&result_id)
    } else {
        contains_poison(inst, poison)
    }
}

pub fn poison_pass(module: &mut Module, poison: &mut HashSet<Word>) {
    // TODO: This is O(n^2), can we speed it up?
    while spread_poison(module, poison) {}

    if option_env!("PRINT_POISON").is_some() {
        for f in &module.functions {
            if is_poison(f.def.as_ref().unwrap(), poison) {
                let name_id = f.def.as_ref().unwrap().result_id.unwrap();
                let name = module.debugs.iter().find(|inst| {
                    inst.class.opcode == Op::Name && inst.operands[0] == Operand::IdRef(name_id)
                });
                let name = match name {
                    Some(Instruction { ref operands, .. }) => match operands as &[Operand] {
                        [_, Operand::LiteralString(name)] => name.clone(),
                        _ => panic!(),
                    },
                    _ => format!("{}", name_id),
                };
                println!("Function removed: {}", name)
            }
        }
    }
    module.capabilities.retain(|inst| !is_poison(inst, poison));
    module.extensions.retain(|inst| !is_poison(inst, poison));
    module
        .ext_inst_imports
        .retain(|inst| !is_poison(inst, poison));
    if module
        .memory_model
        .as_ref()
        .map_or(false, |inst| is_poison(inst, poison))
    {
        module.memory_model = None;
    }
    module.entry_points.retain(|inst| !is_poison(inst, poison));
    module
        .execution_modes
        .retain(|inst| !is_poison(inst, poison));
    module.debugs.retain(|inst| !is_poison(inst, poison));
    module.annotations.retain(|inst| !is_poison(inst, poison));
    module
        .types_global_values
        .retain(|inst| !is_poison(inst, poison));
    module
        .functions
        .retain(|f| !is_poison(f.def.as_ref().unwrap(), poison));
}

fn spread_poison(module: &mut Module, poison: &mut HashSet<Word>) -> bool {
    let mut any = false;
    // globals are easy
    for inst in module.global_inst_iter() {
        if let Some(result_id) = inst.result_id {
            if contains_poison(inst, poison) && poison.insert(result_id) {
                any = true;
            }
        }
    }
    // function IDs implicitly reference their contents
    for func in &module.functions {
        let mut func_poisoned = false;
        let mut spread_func = |inst: &Instruction| {
            if let Some(result_id) = inst.result_id {
                if contains_poison(inst, poison) {
                    if poison.insert(result_id) {
                        any = true;
                    }
                    func_poisoned = true;
                } else if poison.contains(&result_id) {
                    func_poisoned = true;
                }
            } else if is_poison(inst, poison) {
                func_poisoned = true;
            }
        };
        for param in &func.parameters {
            spread_func(param);
        }
        for block in &func.blocks {
            for inst in &block.label {
                spread_func(inst);
            }
            for inst in &block.instructions {
                spread_func(inst);
            }
        }
        for inst in &func.end {
            spread_func(inst);
        }
        if func_poisoned && poison.insert(func.def.as_ref().unwrap().result_id.unwrap()) {
            any = true;
        }
    }
    any
}
