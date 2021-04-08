//! See documentation on `CodegenCx::zombie` for a description of the zombie system.

use crate::decorations::{CustomDecoration, ZombieDecoration};
use rspirv::dr::{Instruction, Module};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::FxHashMap;
use rustc_session::Session;
use rustc_span::{Span, DUMMY_SP};
use std::env;
use std::iter::once;

#[derive(Clone)]
struct ZombieInfo<'a> {
    reason: &'a str,
    span: Span,
    stack: Vec<Word>,
}

impl<'a> ZombieInfo<'a> {
    fn new(reason: &'a str, span: Span) -> Self {
        Self {
            reason,
            span,
            stack: Vec::new(),
        }
    }
    fn push_stack(&self, word: Word) -> Self {
        Self {
            reason: self.reason,
            span: self.span,
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

fn spread_zombie(module: &mut Module, zombie: &mut FxHashMap<Word, ZombieInfo<'_>>) -> bool {
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
            let pushed_reason = reason.push_stack(func_id);
            zombie.insert(func_id, pushed_reason);
            any = true;
        }
    }
    any
}

fn get_names(module: &Module) -> FxHashMap<Word, &str> {
    module
        .debugs
        .iter()
        .filter(|i| i.class.opcode == Op::Name)
        .map(|i| {
            (
                i.operands[0].unwrap_id_ref(),
                i.operands[1].unwrap_literal_string(),
            )
        })
        .collect()
}

// If an entry point references a zombie'd value, then the entry point would normally get removed.
// That's an absolutely horrible experience to debug, though, so instead, create a nice error
// message containing the stack trace of how the entry point got to the zombie value.
fn report_error_zombies(sess: &Session, module: &Module, zombie: &FxHashMap<Word, ZombieInfo<'_>>) {
    let mut names = None;
    for root in super::dce::collect_roots(module) {
        if let Some(reason) = zombie.get(&root) {
            let names = names.get_or_insert_with(|| get_names(module));
            let stack = reason.stack.iter().map(|s| {
                names
                    .get(s)
                    .map(|&n| n.to_string())
                    .unwrap_or_else(|| format!("Unnamed function ID %{}", s))
            });
            let stack_note = once("Stack:".to_string())
                .chain(stack)
                .collect::<Vec<_>>()
                .join("\n");
            sess.struct_span_err(reason.span, reason.reason)
                .note(&stack_note)
                .emit();
        }
    }
}

pub fn remove_zombies(sess: &Session, module: &mut Module) {
    let zombies_owned = ZombieDecoration::decode_all(module)
        .map(|(id, zombie)| {
            let ZombieDecoration { reason, span } = zombie.deserialize();
            let span = span
                .and_then(|span| span.to_rustc(sess.source_map()))
                .unwrap_or(DUMMY_SP);
            (id, (reason, span))
        })
        .collect::<Vec<_>>();
    let mut zombies = zombies_owned
        .iter()
        .map(|(id, (reason, span))| (*id, ZombieInfo::new(reason, *span)))
        .collect();
    ZombieDecoration::remove_all(module);
    // Note: This is O(n^2).
    while spread_zombie(module, &mut zombies) {}

    report_error_zombies(sess, module, &zombies);

    if env::var("PRINT_ALL_ZOMBIE").is_ok() {
        for (&zomb, reason) in &zombies {
            let orig = if zombies_owned.iter().any(|&(z, _)| z == zomb) {
                "original"
            } else {
                "infected"
            };
            println!("zombie'd {} because {} ({})", zomb, reason.reason, orig);
        }
    }

    if env::var("PRINT_ZOMBIE").is_ok() {
        for f in &module.functions {
            if let Some(reason) = is_zombie(f.def.as_ref().unwrap(), &zombies) {
                let name_id = f.def_id().unwrap();
                let name = module.debugs.iter().find(|inst| {
                    inst.class.opcode == Op::Name && inst.operands[0].unwrap_id_ref() == name_id
                });
                let name = match name {
                    Some(Instruction { ref operands, .. }) => {
                        operands[1].unwrap_literal_string().to_string()
                    }
                    _ => format!("{}", name_id),
                };
                println!("Function removed {:?} because {:?}", name, reason.reason)
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
