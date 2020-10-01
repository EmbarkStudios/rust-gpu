use crate::operand_idref_mut;
use rspirv::dr::{Block, Function, Module, Operand};
use rspirv::spirv::{Op, Word};
use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::mem::replace;

pub fn shift_ids(module: &mut Module, add: u32) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_id) = &mut inst.result_id {
            *result_id += add;
        }

        if let Some(ref mut result_type) = &mut inst.result_type {
            *result_type += add;
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = operand_idref_mut(op) {
                *w += add
            }
        })
    });
}

/// spir-v requires basic blocks to be ordered so that if A dominates B, A appears before B (except
/// in the case of backedges). Reverse post-order is a good ordering that satisfies this condition
/// (with an "already visited set" that blocks going deeper, which solves both the fact that it's a
/// DAG, not a tree, as well as backedges).
pub fn block_ordering_pass(func: &mut Function) {
    if func.blocks.len() < 2 {
        return;
    }
    fn visit_postorder(
        func: &Function,
        visited: &mut HashSet<Word>,
        postorder: &mut Vec<Word>,
        current: Word,
    ) {
        if !visited.insert(current) {
            return;
        }
        let current_block = func.blocks.iter().find(|b| label_of(b) == current).unwrap();
        // Reverse the order, so reverse-postorder keeps things tidy
        for &outgoing in outgoing_edges(current_block).iter().rev() {
            visit_postorder(func, visited, postorder, outgoing);
        }
        postorder.push(current);
    }

    let mut visited = HashSet::new();
    let mut postorder = Vec::new();

    let entry_label = label_of(&func.blocks[0]);
    visit_postorder(func, &mut visited, &mut postorder, entry_label);

    let mut old_blocks = replace(&mut func.blocks, Vec::new());
    // Order blocks according to reverse postorder
    for &block in postorder.iter().rev() {
        let index = old_blocks
            .iter()
            .position(|b| label_of(b) == block)
            .unwrap();
        func.blocks.push(old_blocks.remove(index));
    }
    // Note: if old_blocks isn't empty here, that means there were unreachable blocks that were deleted.
    assert_eq!(label_of(&func.blocks[0]), entry_label);
}

fn label_of(block: &Block) -> Word {
    block.label.as_ref().unwrap().result_id.unwrap()
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

pub fn compact_ids(module: &mut Module) -> u32 {
    let mut remap = HashMap::new();

    let mut insert = |current_id: u32| -> u32 {
        let len = remap.len();
        *remap.entry(current_id).or_insert_with(|| len as u32 + 1)
    };

    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_id) = &mut inst.result_id {
            *result_id = insert(*result_id);
        }

        if let Some(ref mut result_type) = &mut inst.result_type {
            *result_type = insert(*result_type);
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = operand_idref_mut(op) {
                *w = insert(*w);
            }
        })
    });

    remap.len() as u32 + 1
}

pub fn sort_globals(module: &mut Module) {
    // Function declarations come before definitions. TODO: Figure out if it's even possible to
    // have a function declaration without a body in a fully linked module?
    module.functions.sort_by_key(|f| !f.blocks.is_empty());
}
