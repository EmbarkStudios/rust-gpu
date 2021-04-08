use rspirv::dr::{Block, Function, Module};
use rspirv::spirv::{Op, Word};
use std::collections::{HashMap, HashSet};
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
            if let Some(w) = op.id_ref_any_mut() {
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
        let current_block = func
            .blocks
            .iter()
            .find(|b| b.label_id().unwrap() == current)
            .unwrap();
        let mut edges = outgoing_edges(current_block).collect::<Vec<_>>();
        // HACK(eddyb) treat `OpSelectionMerge` as an edge, in case it points
        // to an otherwise-unreachable block.
        if let Some(before_last_idx) = current_block.instructions.len().checked_sub(2) {
            if let Some(before_last) = current_block.instructions.get(before_last_idx) {
                if before_last.class.opcode == Op::SelectionMerge {
                    edges.push(before_last.operands[0].unwrap_id_ref());
                }
            }
        }
        // Reverse the order, so reverse-postorder keeps things tidy
        for &outgoing in edges.iter().rev() {
            visit_postorder(func, visited, postorder, outgoing);
        }
        postorder.push(current);
    }

    let mut visited = HashSet::new();
    let mut postorder = Vec::new();

    let entry_label = func.blocks[0].label_id().unwrap();
    visit_postorder(func, &mut visited, &mut postorder, entry_label);

    let mut old_blocks = replace(&mut func.blocks, Vec::new());
    // Order blocks according to reverse postorder
    for &block in postorder.iter().rev() {
        let index = old_blocks
            .iter()
            .position(|b| b.label_id().unwrap() == block)
            .unwrap();
        func.blocks.push(old_blocks.remove(index));
    }
    // Note: if old_blocks isn't empty here, that means there were unreachable blocks that were deleted.
    assert_eq!(func.blocks[0].label_id().unwrap(), entry_label);
}

pub fn outgoing_edges(block: &Block) -> impl Iterator<Item = Word> + '_ {
    let terminator = block.instructions.last().unwrap();
    // https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#Termination
    let operand_indices = match terminator.class.opcode {
        Op::Branch => (0..1).step_by(1),
        Op::BranchConditional => (1..3).step_by(1),
        Op::Switch => (1..terminator.operands.len()).step_by(2),
        Op::Return | Op::ReturnValue | Op::Kill | Op::Unreachable => (0..0).step_by(1),
        _ => panic!("Invalid block terminator: {:?}", terminator),
    };
    operand_indices.map(move |i| terminator.operands[i].unwrap_id_ref())
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
            if let Some(w) = op.id_ref_any_mut() {
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
