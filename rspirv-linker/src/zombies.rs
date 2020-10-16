//! See documentation on CodegenCx::zombie for a description of the zombie system.

use rspirv::dr::{Instruction, Module};
use rspirv::spirv::{Decoration, Op, Word};
use std::collections::{hash_map, HashMap};
use std::env;

fn collect_zombies(module: &Module) -> Vec<(Word, String)> {
    module
        .annotations
        .iter()
        .filter_map(|inst| {
            // TODO: Temp hack. We hijack UserTypeGOOGLE right now, since the compiler never emits this.
            if inst.class.opcode == Op::DecorateString
                && inst.operands[1].unwrap_decoration() == Decoration::UserTypeGOOGLE
            {
                let id = inst.operands[0].unwrap_id_ref();
                let reason = inst.operands[2].unwrap_literal_string();
                return Some((id, reason.to_string()));
            }
            None
        })
        .collect()
}

fn remove_zombie_annotations(module: &mut Module) {
    module.annotations.retain(|inst| {
        inst.class.opcode != Op::DecorateString
            || inst.operands[1].unwrap_decoration() != Decoration::UserTypeGOOGLE
    })
}

fn contains_zombie<'a>(inst: &Instruction, zombie: &HashMap<Word, &'a str>) -> Option<&'a str> {
    if let Some(result_type) = inst.result_type {
        if let Some(reason) = zombie.get(&result_type).copied() {
            return Some(reason);
        }
    }
    inst.operands
        .iter()
        .find_map(|op| op.id_ref_any().and_then(|w| zombie.get(&w).copied()))
}

fn is_zombie<'a>(inst: &Instruction, zombie: &HashMap<Word, &'a str>) -> Option<&'a str> {
    if let Some(result_id) = inst.result_id {
        zombie.get(&result_id).copied()
    } else {
        contains_zombie(inst, zombie)
    }
}

fn spread_zombie(module: &mut Module, zombie: &mut HashMap<Word, &str>) -> bool {
    let mut any = false;
    // globals are easy
    for inst in module.global_inst_iter() {
        if let Some(result_id) = inst.result_id {
            if let Some(reason) = contains_zombie(inst, zombie) {
                match zombie.entry(result_id) {
                    hash_map::Entry::Vacant(entry) => {
                        entry.insert(reason);
                        any = true;
                    }
                    hash_map::Entry::Occupied(_) => {}
                }
            }
        }
    }
    // function IDs implicitly reference their contents
    for func in &module.functions {
        let mut func_is_zombie = None;
        let mut spread_func = |inst: &Instruction| {
            if let Some(result_id) = inst.result_id {
                if let Some(reason) = contains_zombie(inst, zombie) {
                    match zombie.entry(result_id) {
                        hash_map::Entry::Vacant(entry) => {
                            entry.insert(reason);
                            any = true;
                        }
                        hash_map::Entry::Occupied(_) => {}
                    }
                    func_is_zombie = Some(func_is_zombie.unwrap_or(reason));
                } else if let Some(reason) = zombie.get(&result_id) {
                    func_is_zombie = Some(func_is_zombie.unwrap_or(reason));
                }
            } else if let Some(reason) = is_zombie(inst, zombie) {
                func_is_zombie = Some(func_is_zombie.unwrap_or(reason));
            }
        };
        for def in &func.def {
            spread_func(def);
        }
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
        if let Some(reason) = func_is_zombie {
            match zombie.entry(func.def.as_ref().unwrap().result_id.unwrap()) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(reason);
                    any = true;
                }
                hash_map::Entry::Occupied(_) => {}
            }
        }
    }
    any
}

pub fn remove_zombies(module: &mut Module) {
    let zombies_owned = collect_zombies(module);
    let mut zombies = zombies_owned.iter().map(|(a, b)| (*a, b as &str)).collect();
    remove_zombie_annotations(module);
    // Note: This is O(n^2).
    while spread_zombie(module, &mut zombies) {}

    if env::var("PRINT_ALL_ZOMBIE").is_ok() {
        for (&zomb, &reason) in &zombies {
            let orig = if zombies_owned.iter().any(|&(z, _)| z == zomb) {
                "original"
            } else {
                "infected"
            };
            println!("zombie'd {} because {} ({})", zomb, reason, orig);
        }
    }

    if env::var("PRINT_ZOMBIE").is_ok() {
        for f in &module.functions {
            if let Some(reason) = is_zombie(f.def.as_ref().unwrap(), &zombies) {
                let name_id = f.def.as_ref().unwrap().result_id.unwrap();
                let name = module.debugs.iter().find(|inst| {
                    inst.class.opcode == Op::Name && inst.operands[0].unwrap_id_ref() == name_id
                });
                let name = match name {
                    Some(Instruction { ref operands, .. }) => {
                        operands[1].unwrap_literal_string().to_string()
                    }
                    _ => format!("{}", name_id),
                };
                println!("Function removed {:?} because {:?}", name, reason)
            }
        }
    }

    module
        .capabilities
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .extensions
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .ext_inst_imports
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    if module
        .memory_model
        .as_ref()
        .map_or(false, |inst| is_zombie(inst, &zombies).is_some())
    {
        module.memory_model = None;
    }
    module
        .entry_points
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .execution_modes
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .debugs
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .annotations
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .types_global_values
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .functions
        .retain(|f| is_zombie(f.def.as_ref().unwrap(), &zombies).is_none());
}
