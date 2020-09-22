use rspirv::dr::{Block, Function, Instruction, Module, ModuleHeader, Operand};
use rspirv::spirv::{Op, Word};
use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::mem::replace;

pub fn export_zombies(module: &mut Module, zombies: &HashMap<Word, &'static str>) {
    fn gen_id(header: &mut Option<ModuleHeader>) -> Word {
        let header = match header {
            Some(h) => h,
            None => panic!(),
        };
        let id = header.bound;
        header.bound += 1;
        id
    }
    for (id, reason) in zombies {
        let str = format!("rustc_codegen_spirv_zombie={}:{}", id, reason);
        let dummy_id = gen_id(&mut module.header);
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
