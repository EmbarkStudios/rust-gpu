//! See documentation on `CodegenCx::zombie` for a description of the zombie system.

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

fn is_or_contains_zombie<'a>(
    inst: &Instruction,
    zombie: &HashMap<Word, &'a str>,
) -> Option<&'a str> {
    let result_zombie = inst
        .result_id
        .and_then(|result_id| zombie.get(&result_id).copied());
    result_zombie.or_else(|| contains_zombie(inst, zombie))
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
    // No need to zombie defs within a function: If any def within a function is zombied, then the
    // whole function is zombied. But, we don't have to mark the defs within a function as zombie,
    // because the defs can't escape the function.
    for func in &module.functions {
        let func_id = func.def_id().unwrap();
        // Can't use zombie.entry() here, due to using the map in contains_zombie
        if zombie.contains_key(&func_id) {
            // Func is already zombie, no need to scan it again.
            continue;
        }
        let func_is_zombie = func
            .all_inst_iter()
            .find_map(|inst| is_or_contains_zombie(inst, zombie));
        if let Some(reason) = func_is_zombie {
            zombie.insert(func_id, reason);
            any = true;
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
