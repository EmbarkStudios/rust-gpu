use rspirv::dr::{Block, Function, Instruction, Module, Operand};
use rspirv::spirv::{Op, Word};
use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::mem::replace;

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
    // Note: This is O(n^2).
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
        if func_poisoned && poison.insert(func.def.as_ref().unwrap().result_id.unwrap()) {
            any = true;
        }
    }
    any
}

// https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
pub fn block_ordering_pass(func: &mut Function) {
    if func.blocks.len() < 2 {
        return;
    }
    let mut graph = func
        .blocks
        .iter()
        .map(|block| {
            (
                block.label.as_ref().unwrap().result_id.unwrap(),
                outgoing_edges(block),
            )
        })
        .collect();
    let entry_label = func.blocks[0].label.as_ref().unwrap().result_id.unwrap();
    delete_backedges(&mut graph, entry_label);

    let mut sorter = topological_sort::TopologicalSort::<Word>::new();
    for (key, values) in graph {
        for value in values {
            sorter.add_dependency(key, value);
        }
    }
    let mut old_blocks = replace(&mut func.blocks, Vec::new());
    while let Some(item) = sorter.pop() {
        let index = old_blocks
            .iter()
            .position(|b| b.label.as_ref().unwrap().result_id.unwrap() == item)
            .unwrap();
        func.blocks.push(old_blocks.remove(index));
    }
    assert!(sorter.is_empty());
    assert!(old_blocks.is_empty());
    assert_eq!(
        func.blocks[0].label.as_ref().unwrap().result_id.unwrap(),
        entry_label,
        "Topo sorter did something weird (unreachable blocks?)"
    );
}

fn outgoing_edges(block: &Block) -> Vec<Word> {
    fn unwrap_id_ref(operand: &Operand) -> Word {
        match *operand {
            Operand::IdRef(word) => word,
            _ => panic!("Expected Operand::IdRef: {}", operand),
        }
    }
    let terminator = block.instructions.last().unwrap();
    // https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#Termination
    match terminator.class.opcode {
        Op::Branch => vec![unwrap_id_ref(&terminator.operands[0])],
        Op::BranchConditional => vec![
            unwrap_id_ref(&terminator.operands[1]),
            unwrap_id_ref(&terminator.operands[2]),
        ],
        Op::Switch => once(unwrap_id_ref(&terminator.operands[1]))
            .chain(
                terminator.operands[3..]
                    .iter()
                    .step_by(2)
                    .map(unwrap_id_ref),
            )
            .collect(),
        Op::Return | Op::ReturnValue | Op::Kill | Op::Unreachable => Vec::new(),
        _ => panic!("Invalid block terminator: {:?}", terminator),
    }
}

fn delete_backedges(graph: &mut HashMap<Word, Vec<Word>>, entry: Word) {
    // TODO: This has extremely bad runtime
    let mut backedges = HashSet::new();
    fn re(
        graph: &HashMap<Word, Vec<Word>>,
        entry: Word,
        stack: &mut Vec<Word>,
        backedges: &mut HashSet<(Word, Word)>,
    ) {
        stack.push(entry);
        for &item in &graph[&entry] {
            if stack.contains(&item) {
                backedges.insert((entry, item));
            } else if !backedges.contains(&(entry, item)) {
                re(graph, item, stack, backedges);
            }
        }
        assert_eq!(stack.pop(), Some(entry));
    }
    re(graph, entry, &mut Vec::new(), &mut backedges);
    for (from, to) in backedges {
        graph.get_mut(&from).unwrap().retain(|&o| o != to);
    }
}
