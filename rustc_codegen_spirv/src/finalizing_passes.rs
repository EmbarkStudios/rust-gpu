use rspirv::dr::{Block, Function, Instruction, Module, ModuleHeader, Operand};
use rspirv::spirv::{Decoration, LinkageType, Op, Word};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::mem::replace;

fn contains_zombie(
    inst: &Instruction,
    zombie: &HashMap<Word, &'static str>,
) -> Option<&'static str> {
    inst.result_type.map_or_else(
        || {
            inst.operands.iter().find_map(|op| match op {
                rspirv::dr::Operand::IdMemorySemantics(w)
                | rspirv::dr::Operand::IdScope(w)
                | rspirv::dr::Operand::IdRef(w) => zombie.get(w).copied(),
                _ => None,
            })
        },
        |w| zombie.get(&w).copied(),
    )
}

fn is_zombie(inst: &Instruction, zombie: &HashMap<Word, &'static str>) -> Option<&'static str> {
    if let Some(result_id) = inst.result_id {
        zombie.get(&result_id).copied()
    } else {
        contains_zombie(inst, zombie)
    }
}

pub fn zombie_pass(module: &mut Module, zombie: &mut HashMap<Word, &'static str>) {
    // Note: This is O(n^2).
    while spread_zombie(module, zombie) {}

    export_zombies(module, zombie);

    if option_env!("PRINT_ZOMBIE").is_some() {
        for f in &module.functions {
            if let Some(reason) = is_zombie(f.def.as_ref().unwrap(), zombie) {
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
                println!("Function removed {:?} because {:?}", name, reason)
            }
        }
    }
    module
        .capabilities
        .retain(|inst| is_zombie(inst, zombie).is_none());
    module
        .extensions
        .retain(|inst| is_zombie(inst, zombie).is_none());
    module
        .ext_inst_imports
        .retain(|inst| is_zombie(inst, zombie).is_none());
    if module
        .memory_model
        .as_ref()
        .map_or(false, |inst| is_zombie(inst, zombie).is_some())
    {
        module.memory_model = None;
    }
    module
        .entry_points
        .retain(|inst| is_zombie(inst, zombie).is_none());
    module
        .execution_modes
        .retain(|inst| is_zombie(inst, zombie).is_none());
    module
        .debugs
        .retain(|inst| is_zombie(inst, zombie).is_none());
    module
        .annotations
        .retain(|inst| is_zombie(inst, zombie).is_none());
    module
        .types_global_values
        .retain(|inst| is_zombie(inst, zombie).is_none());
    module
        .functions
        .retain(|f| is_zombie(f.def.as_ref().unwrap(), zombie).is_none());
}

fn spread_zombie(module: &mut Module, zombie: &mut HashMap<Word, &'static str>) -> bool {
    let mut any = false;
    // globals are easy
    for inst in module.global_inst_iter() {
        if let Some(result_id) = inst.result_id {
            if let Some(reason) = contains_zombie(inst, zombie) {
                match zombie.entry(result_id) {
                    Entry::Vacant(entry) => {
                        entry.insert(reason);
                        any = true;
                    }
                    Entry::Occupied(_) => {}
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
                        Entry::Vacant(entry) => {
                            entry.insert(reason);
                            any = true;
                        }
                        Entry::Occupied(_) => {}
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
                Entry::Vacant(entry) => {
                    entry.insert(reason);
                    any = true;
                }
                Entry::Occupied(_) => {}
            }
        }
    }
    any
}

fn export_zombies(module: &mut Module, zombie: &HashMap<Word, &'static str>) {
    fn id(header: &mut Option<ModuleHeader>) -> Word {
        let header = match header {
            Some(h) => h,
            None => panic!(),
        };
        let id = header.bound;
        header.bound += 1;
        id
    }
    for inst in &module.annotations {
        if inst.class.opcode == Op::Decorate
            && inst.operands[1] == Operand::Decoration(Decoration::LinkageAttributes)
            && inst.operands[3] == Operand::LinkageType(LinkageType::Export)
        {
            let (target, name) = match (&inst.operands[0], &inst.operands[2]) {
                (&Operand::IdRef(id), Operand::LiteralString(name)) => (id, name),
                _ => panic!(),
            };
            if zombie.contains_key(&target) {
                let str = format!("rustc_codegen_spirv_export_zombie_symbol={}", name);
                let dummy_id = id(&mut module.header);
                // TODO: OpString must come before other sections in the debug group. Fix rspirv here.
                module.debugs.insert(
                    0,
                    Instruction::new(
                        Op::String,
                        None,
                        Some(dummy_id),
                        vec![Operand::LiteralString(str)],
                    ),
                );
            }
        }
    }
}

fn label_of(block: &Block) -> Word {
    block.label.as_ref().unwrap().result_id.unwrap()
}

pub fn delete_dead_blocks(func: &mut Function) {
    if func.blocks.len() < 2 {
        return;
    }
    let entry_label = label_of(&func.blocks[0]);
    let mut stack = Vec::new();
    let mut visited = HashSet::new();
    stack.push(entry_label);
    visited.insert(entry_label);
    while let Some(label) = stack.pop() {
        let block = func.blocks.iter().find(|b| label_of(b) == label).unwrap();
        for outgoing in outgoing_edges(block) {
            if visited.insert(outgoing) {
                stack.push(outgoing);
            }
        }
    }
    func.blocks.retain(|b| visited.contains(&label_of(b)))
}

pub fn block_ordering_pass(func: &mut Function) {
    if func.blocks.len() < 2 {
        return;
    }
    let mut graph = func
        .blocks
        .iter()
        .map(|block| (label_of(block), outgoing_edges(block)))
        .collect();
    let entry_label = label_of(&func.blocks[0]);
    delete_backedges(&mut graph, entry_label);

    let mut sorter = topological_sort::TopologicalSort::<Word>::new();
    for (key, values) in graph {
        for value in values {
            sorter.add_dependency(key, value);
        }
    }
    let mut old_blocks = replace(&mut func.blocks, Vec::new());
    while let Some(item) = sorter.pop() {
        let index = old_blocks.iter().position(|b| label_of(b) == item).unwrap();
        func.blocks.push(old_blocks.remove(index));
    }
    assert!(sorter.is_empty());
    assert!(old_blocks.is_empty());
    assert_eq!(
        label_of(&func.blocks[0]),
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
