// This pass inserts merge instructions for structured control flow with the assumption the spir-v is reducible.

use super::id;
use super::simple_passes::outgoing_edges;
use rspirv::spirv::{Op, Word};
use rspirv::{
    dr::{Block, Function, Instruction, ModuleHeader, Operand},
    spirv::SelectionControl,
};
use rustc_session::Session;
use std::collections::VecDeque;

pub fn structurize(sess: Option<&Session>, header: &mut ModuleHeader, func: &mut Function) {
    insert_selection_merge_on_conditional_branch(sess, header, &mut func.blocks);
}

fn find_block_index_from_id(blocks: &[Block], id: &Word) -> usize {
    for (i, block) in blocks.iter().enumerate() {
        if block.label_id() == Some(*id) {
            return i;
        }
    }

    panic!("Failed to find block from id");
}

fn get_possible_merge_positions(blocks: &[Block], start: Word) -> Vec<usize> {
    let mut retval = Vec::new();
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(start);

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(blocks, &front);
        let new_edges = outgoing_edges(&blocks[block_idx]);

        // We found a possible merge position
        if new_edges.len() == 1 {
            retval.push(find_block_index_from_id(blocks, &new_edges[0]));
        }

        next.extend(new_edges);
    }

    retval
}

fn block_is_loop(blocks: &[Block], start: Word) -> bool {
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(start);

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(blocks, &front);
        let new_edges = outgoing_edges(&blocks[block_idx]);

        if new_edges.contains(&start) {
            return true;
        }

        next.extend(new_edges);
    }

    false
}

fn ends_in_return(block: &Block) -> bool {
    let last_inst = block.instructions.last().unwrap();
    last_inst.class.opcode == Op::Return || last_inst.class.opcode == Op::ReturnValue
}

pub fn insert_selection_merge_on_conditional_branch(
    sess: Option<&Session>,
    header: &mut ModuleHeader,
    blocks: &mut [Block],
) {
    let mut branch_conditional_ops = Vec::new();

    // Find conditional branch
    for (bi, block) in blocks.iter().enumerate() {
        if block_is_loop(blocks, block.label_id().unwrap()) {
            if let Some(sess) = sess {
                sess.err("Loops are unsupported");
            }
        }

        for (ii, inst) in block.instructions.iter().enumerate() {
            if inst.class.opcode == Op::BranchConditional {
                branch_conditional_ops.push((bi, ii));
            }
        }
    }

    // Find convergence point.
    for (bi, ii) in branch_conditional_ops {
        let out = outgoing_edges(&blocks[bi]);
        if out.len() != 2 {
            panic!("Viktor missunderstood something");
        }
        let a_nexts = get_possible_merge_positions(blocks, out[0]);
        let b_nexts = get_possible_merge_positions(blocks, out[1]);

        // Check for a matching possible merge position.
        let mut first_merge = None;
        'outer: for a in &a_nexts {
            for b in &b_nexts {
                if *a == *b {
                    first_merge = Some(*a);
                    break 'outer;
                }
            }
        }

        let selection_merge_operands = if let Some(idx) = first_merge {
            // We found a existing block that we can use as a merge block!
            let merge_block_id = blocks[idx].label_id().unwrap();
            vec![
                Operand::IdRef(merge_block_id),
                Operand::SelectionControl(SelectionControl::NONE),
            ]
        } else {
            // insert a new block that might be unreachable but that is okay.
            // I think this can only happen if one of the branches returns from the func
            // or inside of a loop, this is a break/continue so the merge block
            // doesn't become unreachable but instead branches to the for loops merge/continue block.
            // AKA, first do the for loops, then do the conditional branch instructions.
            let end = if ends_in_return(&blocks[*a_nexts.last().unwrap()]) {
                &mut blocks[*b_nexts.last().unwrap()]
            } else {
                &mut blocks[*a_nexts.last().unwrap()]
            };

            let mut new_block = Block::new();
            new_block.label = end.label.clone();
            let new_end_block_id = id(header);
            end.label.as_mut().unwrap().result_id = Some(new_end_block_id);

            let branch_inst = Instruction::new(
                Op::Branch,
                None,
                None,
                vec![Operand::IdRef(new_end_block_id)],
            );
            new_block.instructions.push(branch_inst);

            vec![
                Operand::IdRef(new_block.label.unwrap().result_id.unwrap()),
                Operand::SelectionControl(SelectionControl::NONE),
            ]
        };

        // Insert the merge instruction
        let block = &mut blocks[bi];
        let merge_inst = Instruction::new(Op::SelectionMerge, None, None, selection_merge_operands);
        block.instructions.insert(ii, merge_inst);
    }
}
