//! See documentation on `CodegenCx::zombie` for a description of the zombie system.

use super::{get_name, get_names};
use crate::decorations::{CustomDecoration, LazilyDeserialized, SpanRegenerator, ZombieDecoration};
use rspirv::dr::{Instruction, Module};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::FxHashMap;
use rustc_session::Session;
use rustc_span::DUMMY_SP;
use std::iter::once;

// FIXME(eddyb) change this to chain through IDs instead of wasting allocations.
#[derive(Clone)]
struct ZombieInfo<'a> {
    serialized: &'a LazilyDeserialized<'static, ZombieDecoration>,
    stack: Vec<Word>,
}

impl<'a> ZombieInfo<'a> {
    fn push_stack(&self, word: Word) -> Self {
        Self {
            serialized: self.serialized,
            stack: self.stack.iter().cloned().chain(once(word)).collect(),
        }
    }
}

fn contains_zombie<'h, 'a>(
    inst: &Instruction,
    zombie: &'h FxHashMap<Word, ZombieInfo<'a>>,
) -> Option<&'h ZombieInfo<'a>> {
    if let Some(result_type) = inst.result_type {
        if let Some(reason) = zombie.get(&result_type) {
            return Some(reason);
        }
    }
    inst.operands
        .iter()
        .find_map(|op| op.id_ref_any().and_then(|w| zombie.get(&w)))
}

fn is_zombie<'h, 'a>(
    inst: &Instruction,
    zombie: &'h FxHashMap<Word, ZombieInfo<'a>>,
) -> Option<&'h ZombieInfo<'a>> {
    if let Some(result_id) = inst.result_id {
        zombie.get(&result_id)
    } else {
        contains_zombie(inst, zombie)
    }
}

fn is_or_contains_zombie<'h, 'a>(
    inst: &Instruction,
    zombie: &'h FxHashMap<Word, ZombieInfo<'a>>,
) -> Option<&'h ZombieInfo<'a>> {
    let result_zombie = inst.result_id.and_then(|result_id| zombie.get(&result_id));
    result_zombie.or_else(|| contains_zombie(inst, zombie))
}

fn spread_zombie(module: &Module, zombie: &mut FxHashMap<Word, ZombieInfo<'_>>) -> bool {
    let mut any = false;
    // globals are easy
    for inst in module.global_inst_iter() {
        if let Some(result_id) = inst.result_id {
            if let Some(reason) = contains_zombie(inst, zombie) {
                if zombie.contains_key(&result_id) {
                    continue;
                }
                let reason = reason.clone();
                zombie.insert(result_id, reason);
                any = true;
            }
        }
    }
    // No need to zombie defs within a function: If any def within a function is zombied, then the
    // whole function is zombied. But, we don't have to mark the defs within a function as zombie,
    // because the defs can't escape the function.
    // HACK(eddyb) one exception to this is function-local variables, which may
    // be unused and as such cannot be allowed to always zombie the function.
    for func in &module.functions {
        let func_id = func.def_id().unwrap();
        // Can't use zombie.entry() here, due to using the map in contains_zombie
        if zombie.contains_key(&func_id) {
            // Func is already zombie, no need to scan it again.
            continue;
        }
        for inst in func.all_inst_iter() {
            if inst.class.opcode == Op::Variable {
                let result_id = inst.result_id.unwrap();
                if let Some(reason) = contains_zombie(inst, zombie) {
                    if zombie.contains_key(&result_id) {
                        continue;
                    }
                    let reason = reason.clone();
                    zombie.insert(result_id, reason);
                    any = true;
                }
            } else if let Some(reason) = is_or_contains_zombie(inst, zombie) {
                let pushed_reason = reason.push_stack(func_id);
                zombie.insert(func_id, pushed_reason);
                any = true;
                break;
            }
        }
    }
    any
}

// If an entry point references a zombie'd value, then the entry point would normally get removed.
// That's an absolutely horrible experience to debug, though, so instead, create a nice error
// message containing the stack trace of how the entry point got to the zombie value.
fn report_error_zombies(
    sess: &Session,
    module: &Module,
    zombie: &FxHashMap<Word, ZombieInfo<'_>>,
) -> super::Result<()> {
    let mut span_regen = SpanRegenerator::new(sess.source_map(), module);

    let mut result = Ok(());
    let mut names = None;
    for root in super::dce::collect_roots(module) {
        if let Some(zombie_info) = zombie.get(&root) {
            let ZombieDecoration { reason, span } = zombie_info.serialized.deserialize();
            let span = span
                .and_then(|span| span_regen.serialized_span_to_rustc(&span))
                .unwrap_or(DUMMY_SP);
            let names = names.get_or_insert_with(|| get_names(module));
            let stack = zombie_info
                .stack
                .iter()
                .map(|&s| get_name(names, s).into_owned());
            let stack_note = once("Stack:".to_string())
                .chain(stack)
                .collect::<Vec<_>>()
                .join("\n");
            result = Err(sess.struct_span_err(span, reason).note(&stack_note).emit());
        }
    }
    result
}

pub fn remove_zombies(
    sess: &Session,
    opts: &super::Options,
    module: &mut Module,
) -> super::Result<()> {
    // FIXME(eddyb) combine these two steps to take the original strings,
    // instead of effectively cloning them (via `.into_owned()`).
    let zombies_owned = ZombieDecoration::decode_all(module)
        .map(|(id, zombie)| (id, zombie.into_owned()))
        .collect::<Vec<_>>();
    ZombieDecoration::remove_all(module);

    let mut zombies = zombies_owned
        .iter()
        .map(|(id, serialized)| {
            (
                *id,
                ZombieInfo {
                    serialized,
                    stack: vec![],
                },
            )
        })
        .collect();
    // Note: This is O(n^2).
    while spread_zombie(module, &mut zombies) {}

    let result = report_error_zombies(sess, module, &zombies);

    // FIXME(eddyb) use `log`/`tracing` instead.
    if opts.print_all_zombie {
        for (&zombie_id, zombie_info) in &zombies {
            let orig = if zombies_owned.iter().any(|&(z, _)| z == zombie_id) {
                "original"
            } else {
                "infected"
            };
            println!(
                "zombie'd {} because {} ({})",
                zombie_id,
                zombie_info.serialized.deserialize().reason,
                orig
            );
        }
    }

    if opts.print_zombie {
        let names = get_names(module);
        for f in &module.functions {
            if let Some(zombie_info) = is_zombie(f.def.as_ref().unwrap(), &zombies) {
                let name = get_name(&names, f.def_id().unwrap());
                println!(
                    "Function removed {:?} because {:?}",
                    name,
                    zombie_info.serialized.deserialize().reason
                );
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
        .debug_string_source
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .debug_names
        .retain(|inst| is_zombie(inst, &zombies).is_none());
    module
        .debug_module_processed
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

    result
}
