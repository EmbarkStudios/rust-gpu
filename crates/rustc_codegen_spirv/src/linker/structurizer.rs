// This pass inserts merge instructions for structured control flow with the assumption the spir-v is reducible.

// TODO: Could i simplify break detection by just checking, hey does the start branch branch to a merge block?
// TODO: Verify we are never splitting a block that is queued for structurization.
// TODO: are there any cases where I need to retarget branches or conditional branches when splitting a block?

use super::id;
use super::simple_passes::outgoing_edges;
use rspirv::spirv::{Op, Word};
use rspirv::{
    dr::{Block, Instruction, Module, ModuleHeader, Operand},
    spirv::SelectionControl,
};
use rustc_session::Session;
use std::collections::VecDeque;

pub struct LoopInfo {
    merge_id: Word,
    continue_id: Word,
    header_id: Word,
}
pub struct ControlFlowInfo {
    loops: Vec<LoopInfo>,
    if_merge_ids: Vec<Word>,
    switch_merge_ids: Vec<Word>,
}

impl ControlFlowInfo {
    fn new() -> Self {
        Self {
            loops: Vec::new(),
            if_merge_ids: Vec::new(),
            switch_merge_ids: Vec::new(),
        }
    }

    fn id_is_loops_merge(&self, id: Word) -> bool {
        for loop_info in &self.loops {
            if loop_info.merge_id == id {
                return true;
            }
        }

        false
    }

    fn id_is_loops_header(&self, id: Word) -> bool {
        for loop_info in &self.loops {
            if loop_info.header_id == id {
                return true;
            }
        }

        false
    }

    fn id_is_loops_continue(&self, id: Word) -> bool {
        for loop_info in &self.loops {
            if loop_info.continue_id == id {
                return true;
            }
        }

        false
    }

    fn id_is_ifs_merge(&self, id: Word) -> bool {
        for merge in &self.if_merge_ids {
            if *merge == id {
                return true;
            }
        }

        false
    }

    fn set_loops_continue_and_merge(&mut self, header_id: Word, merge_id: Word, continue_id: Word) {
        for loop_info in &mut self.loops {
            if loop_info.header_id == header_id {
                loop_info.merge_id = merge_id;
                loop_info.continue_id = continue_id;
                return;
            }
        }

        panic!("tried to set the continue and merge of a header block that does not exist");
    }

    fn used(&self, id: Word) -> bool {
        // I don't believe it is nessessary to check if the block is used as a loop header.
        self.id_is_loops_merge(id) || self.id_is_loops_continue(id) || self.id_is_ifs_merge(id)
    }

    fn retarget(&mut self, old: Word, new: Word) {
        for loop_info in &mut self.loops {
            if loop_info.header_id == old {
                loop_info.header_id = new;
            } else if loop_info.merge_id == old {
                loop_info.merge_id = new;
            } else if loop_info.continue_id == old {
                loop_info.continue_id = new;
            }
        }
        for merge_id in &mut self.if_merge_ids {
            if *merge_id == old {
                *merge_id = new;
            }
        }
        for merge_id in &mut self.switch_merge_ids {
            if *merge_id == old {
                *merge_id = new;
            }
        }
    }

    fn get_debug_names(&self) -> Vec<(Word, String)> {
        let mut retval = Vec::new();

        for loop_info in &self.loops {
            retval.push((loop_info.header_id, "loop_header".to_string()));
            retval.push((loop_info.merge_id, "loop_merge".to_string()));
            retval.push((loop_info.continue_id, "loop_continue".to_string()));
        }
        for id in &self.if_merge_ids {
            retval.push((*id, "if_merge".to_string()));
        }
        for id in &self.switch_merge_ids {
            retval.push((*id, "switch_merge".to_string()));
        }

        retval
    }
}

pub fn structurize(sess: Option<&Session>, module: &mut Module) {
    let mut debug_names = Vec::new();

    for func in &mut module.functions {
        let mut cf_info = ControlFlowInfo::new();

        insert_loop_merge_on_conditional_branch(
            &mut module.header.as_mut().unwrap(),
            &mut func.blocks,
            &mut cf_info,
        );

        retarget_loop_children_if_needed(&mut func.blocks, &cf_info);

        insert_selection_merge_on_conditional_branch(
            sess,
            &mut module.header.as_mut().unwrap(),
            &mut func.blocks,
            &mut cf_info,
        );

        debug_names.extend(cf_info.get_debug_names());
    }

    for (id, name) in debug_names {
        module.debugs.push(Instruction::new(
            Op::Name,
            None,
            None,
            vec![Operand::IdRef(id), Operand::LiteralString(name)],
        ))
    }
}

fn find_block_index_from_id(blocks: &[Block], id: &Word) -> usize {
    for (i, block) in blocks.iter().enumerate() {
        if block.label_id() == Some(*id) {
            return i;
        }
    }

    panic!("Failed to find block from id {}", id);
}

// some times break will yeet themselfs out of a parent loop by skipping the merge block. This prevents that.
fn retarget_loop_children_if_needed(blocks: &mut [Block], cf_info: &ControlFlowInfo) {
    for loop_info in &cf_info.loops {
        let LoopInfo {
            header_id: header,
            merge_id: merge,
            ..
        } = loop_info;

        let mut next: VecDeque<Word> = VecDeque::new();
        next.push_back(*header);

        while let Some(front) = next.pop_front() {
            let block_idx = find_block_index_from_id(blocks, &front);
            let mut new_edges = outgoing_edges(&blocks[block_idx]);

            // Make sure we are not looping or going into child loops.
            for loop_info in &cf_info.loops {
                if new_edges.contains(&loop_info.header_id) {
                    let index = new_edges
                        .iter()
                        .position(|x| *x == loop_info.header_id)
                        .unwrap();
                    new_edges.remove(index);
                }
            }

            // don't continue after merge
            if front == *merge {
                new_edges.clear();
            }

            if new_edges.len() == 1 {
                // if front branches to a block that is the child of a merge, retarget it.
                if block_is_parent_of(*merge, new_edges[0], blocks) {
                    // retarget front to branch to merge.
                    let front_block = &mut blocks[block_idx];
                    (*front_block
                        .instructions
                        .last_mut()
                        .unwrap()
                        .operands
                        .last_mut()
                        .unwrap()) = Operand::IdRef(*merge);
                }
            }

            next.extend(new_edges);
        }
    }
}

// "Combines" all continue blocks into 1 and returns the ID of the continue block.
fn eliminate_multiple_continue_blocks(blocks: &mut Vec<Block>, header: Word) -> Word {
    // Find all possible continue blocks.
    let mut continue_blocks = Vec::new();
    for block in blocks.clone() {
        let block_id = block.label_id().unwrap();
        if ends_in_branch(&block) {
            let edge = outgoing_edges(&block)[0];
            if edge == header && block_is_parent_of(header, block_id, blocks) {
                continue_blocks.push(block_id);
            }
        }
    }
    // if there are multiple continue blocks we need to retarget towards a single continue.
    if continue_blocks.len() > 1 {
        let continue_block_id = continue_blocks.last().unwrap();
        for block_id in continue_blocks.iter().take(continue_blocks.len() - 1) {
            let idx = find_block_index_from_id(blocks, block_id);
            let block = &mut blocks[idx];
            for op in &mut block.instructions.last_mut().unwrap().operands {
                if *op == Operand::IdRef(header) {
                    *op = Operand::IdRef(*continue_block_id);
                }
            }
        }

        *continue_block_id
    } else {
        *continue_blocks.last().unwrap()
    }
}

fn block_leads_into_break(blocks: &[Block], cf_info: &ControlFlowInfo, start: Word) -> bool {
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(start);

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(blocks, &front);
        let mut new_edges = outgoing_edges(&blocks[block_idx]);

        // Make sure we are not looping.
        for loop_info in &cf_info.loops {
            if new_edges.contains(&loop_info.header_id) {
                let index = new_edges
                    .iter()
                    .position(|x| *x == loop_info.header_id)
                    .unwrap();
                new_edges.remove(index);
            }
        }

        // Skip inner branches. TODO: is this correct?
        if ends_in_branch_conditional(&blocks[find_block_index_from_id(blocks, &front)]) {
            new_edges.clear();
        }

        // if front is a merge block return true
        for loop_info in &cf_info.loops {
            if front == loop_info.merge_id && block_is_parent_of(loop_info.header_id, start, blocks)
            {
                return true;
            }
        }

        next.extend(new_edges);
    }

    false
}

fn get_possible_merge_positions(
    blocks: &[Block],
    cf_info: &ControlFlowInfo,
    start: Word,
) -> Vec<usize> {
    let mut retval = Vec::new();
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(start);

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(blocks, &front);
        let mut new_edges = outgoing_edges(&blocks[block_idx]);

        // Don't queue the start block if its a edge
        if let Some(i) = new_edges.iter().position(|x| *x == start) {
            new_edges.remove(i);
        }

        for loop_info in &cf_info.loops {
            // Make sure we are not looping.
            if block_is_parent_of(loop_info.header_id, start, blocks)
                && new_edges.contains(&loop_info.header_id)
            {
                let index = new_edges
                    .iter()
                    .position(|x| *x == loop_info.header_id)
                    .unwrap();
                new_edges.remove(index);
            }

            // Make sure we are not continuing after a merge.
            if block_is_parent_of(loop_info.header_id, start, blocks) && front == loop_info.merge_id
            {
                new_edges.clear();
            }
        }

        // We found a possible merge position, make sure it isn't a merge of a loop because in that case we want to use break logic.
        if new_edges.len() == 1 && !cf_info.id_is_loops_merge(new_edges[0]) {
            retval.push(find_block_index_from_id(blocks, &new_edges[0]));
        }

        next.extend(new_edges);
    }

    retval
}

fn block_is_parent_of(parent: Word, child: Word, blocks: &[Block]) -> bool {
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(parent);

    let mut processed = Vec::new();
    processed.push(parent); // ensures we are not looping.

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(blocks, &front);
        let mut new_edges = outgoing_edges(&blocks[block_idx]);

        for id in &processed {
            if let Some(i) = new_edges.iter().position(|x| x == id) {
                new_edges.remove(i);
            }
        }

        if new_edges.contains(&child) {
            return true;
        }

        processed.push(front);
        next.extend(new_edges);
    }

    false
}

// Returns the idx of the branch that loops.
fn get_looping_branch_from_block(
    blocks: &[Block],
    cf_info: &ControlFlowInfo,
    start: Word,
) -> Option<usize> {
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(start);

    let mut processed = Vec::new();

    while let Some(front) = next.pop_front() {
        // make sure we separate inner from outer loops.
        if front != start && cf_info.id_is_loops_header(front) {
            continue;
        }

        let block_idx = find_block_index_from_id(blocks, &front);
        let mut new_edges = outgoing_edges(&blocks[block_idx]);

        let edge_it = new_edges.iter().find(|&x| x == &start); // Check if the new_edges contain the start
        if new_edges.len() == 1 {
            if let Some(edge_it) = edge_it {
                // loop over the orginal edges to find which branch is looping
                let start_edges = outgoing_edges(&blocks[find_block_index_from_id(blocks, &start)]);

                for (i, start_edge) in start_edges.iter().enumerate() {
                    if start_edge == edge_it || block_is_parent_of(*start_edge, *edge_it, blocks) {
                        return Some(i);
                    }
                }
            }
        }

        for id in &processed {
            if let Some(i) = new_edges.iter().position(|x| x == id) {
                new_edges.remove(i);
            }
        }
        processed.push(front);

        next.extend(new_edges);
    }

    None
}

fn ends_in_branch_conditional(block: &Block) -> bool {
    let last_inst = block.instructions.last().unwrap();
    last_inst.class.opcode == Op::BranchConditional
}

fn ends_in_branch(block: &Block) -> bool {
    let last_inst = block.instructions.last().unwrap();
    last_inst.class.opcode == Op::Branch
}

fn ends_in_return(block: &Block) -> bool {
    let last_inst = block.instructions.last().unwrap();
    last_inst.class.opcode == Op::Return || last_inst.class.opcode == Op::ReturnValue
}

// Returns the new id assigned to the original block.
fn split_block(header: &mut ModuleHeader, blocks: &mut Vec<Block>, block_to_split: Word) -> Word {
    // create new block with old id.
    let block_to_split_index = find_block_index_from_id(blocks, &block_to_split);
    let orignial_block = &mut blocks[block_to_split_index];
    let original_id = orignial_block.label_id().unwrap();
    let mut new_block = Block::new();
    new_block.label = orignial_block.label.clone();
    // assign old block new id.
    let new_original_block_id = id(header);
    orignial_block.label.as_mut().unwrap().result_id = Some(new_original_block_id);
    // new block branches to old block.
    let branch_inst = Instruction::new(
        Op::Branch,
        None,
        None,
        vec![Operand::IdRef(new_original_block_id)],
    );
    new_block.instructions.push(branch_inst);
    // update all merge ops to point the the old block with its new id.
    for block in blocks.iter_mut() {
        for inst in &mut block.instructions {
            if inst.class.opcode == Op::LoopMerge || inst.class.opcode == Op::SelectionMerge {
                for operand in &mut inst.operands {
                    if *operand == Operand::IdRef(original_id) {
                        *operand = Operand::IdRef(new_original_block_id);
                    }
                }
            }
        }
    }

    // insert new block before the old block.
    blocks.insert(block_to_split_index, new_block);
    new_original_block_id
}

pub fn insert_selection_merge_on_conditional_branch(
    sess: Option<&Session>,
    header: &mut ModuleHeader,
    blocks: &mut Vec<Block>,
    cf_info: &mut ControlFlowInfo,
) {
    let mut branch_conditional_ops = Vec::new();

    // Find conditional branches that are not loops
    for block in &blocks.clone() {
        if ends_in_branch_conditional(block)
            && !cf_info.id_is_loops_header(block.label_id().unwrap())
        {
            branch_conditional_ops.push(block.label_id().unwrap());
        }
    }

    // Find convergence point.
    for id in branch_conditional_ops.clone() {
        let bi = find_block_index_from_id(blocks, &id);
        let out = outgoing_edges(&blocks[bi]);
        let id = &blocks[bi].label_id().unwrap();
        let a_nexts = get_possible_merge_positions(blocks, cf_info, out[0]);
        let b_nexts = get_possible_merge_positions(blocks, cf_info, out[1]);

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

        let merge_block_id = if let Some(idx) = first_merge {
            // We found a existing block that we can use as a merge block!
            blocks[idx].label_id().unwrap()
        } else {
            let a_first_idx = find_block_index_from_id(blocks, &out[0]);
            let b_first_idx = find_block_index_from_id(blocks, &out[1]);
            let a_first_id = blocks[a_first_idx].label_id().unwrap();
            let b_first_id = blocks[b_first_idx].label_id().unwrap();
            let a_last_idx = match a_nexts.last() {
                Some(last) => *last,
                None => find_block_index_from_id(blocks, &out[0]),
            };
            let b_last_idx = match b_nexts.last() {
                Some(last) => *last,
                None => find_block_index_from_id(blocks, &out[1]),
            };

            let branch_a_breaks = block_leads_into_break(blocks, cf_info, a_first_id);
            let branch_b_breaks = block_leads_into_break(blocks, cf_info, b_first_id);
            let branch_a_returns = ends_in_return(&blocks[a_last_idx]);
            let branch_b_returns = ends_in_return(&blocks[b_last_idx]);

            if branch_a_breaks && branch_b_breaks {
                // (fully unreachable) insert a rando block and mark as merge.
                if let Some(sess) = sess {
                    sess.err("UNIMPLEMENTED, A fully unreachable case was detected.");
                }
                return;
            } else if branch_a_breaks {
                // (partially unreachable) merge block becomes branch b immediatly
                blocks[b_first_idx].label_id().unwrap()
            } else if branch_b_breaks {
                // (partially unreachable) merge block becomes branch a immediatly
                blocks[a_first_idx].label_id().unwrap()
            } else if branch_a_returns {
                // (partially unreachable) merge block becomes end/start of b.
                if let Some(sess) = sess {
                    sess.err("UNIMPLEMENTED, A partially unreachable case was detected on a.");
                }
                return;
            } else if branch_b_returns {
                // (partially unreachable) merge block becomes end/start of a.
                if let Some(sess) = sess {
                    sess.err("UNIMPLEMENTED, A partially unreachable case was detected on b.");
                }
                return;
            } else {
                // (fully unreachable) insert a rando block and mark as merge.
                blocks[b_first_idx].label_id().unwrap()
            }
        };

        if cf_info.used(merge_block_id) {
            let new_id = split_block(header, blocks, merge_block_id);
            cf_info.retarget(merge_block_id, new_id);
        }

        let merge_operands = vec![
            Operand::IdRef(merge_block_id),
            Operand::SelectionControl(SelectionControl::NONE),
        ];

        cf_info.if_merge_ids.push(merge_block_id);

        // Insert the merge instruction
        let bi = find_block_index_from_id(blocks, id); // after this we don't insert or remove blocks
        let block = &mut blocks[bi];
        let merge_inst = Instruction::new(Op::SelectionMerge, None, None, merge_operands);
        block
            .instructions
            .insert(block.instructions.len() - 1, merge_inst);
    }
}

pub fn insert_loop_merge_on_conditional_branch(
    header: &mut ModuleHeader,
    blocks: &mut Vec<Block>,
    cf_info: &mut ControlFlowInfo,
) {
    let mut branch_conditional_ops = Vec::new();

    // Find conditional branches that are loops, and find which branch is the one that loops.
    for (bi, block) in blocks.iter().enumerate() {
        if ends_in_branch_conditional(block) {
            let block_id = block.label_id().unwrap();
            if let Some(looping_branch_idx_and_block_idx) =
                get_looping_branch_from_block(blocks, cf_info, block_id)
            {
                branch_conditional_ops.push((bi, looping_branch_idx_and_block_idx));
                cf_info.loops.push(LoopInfo {
                    header_id: block_id,
                    merge_id: 0,
                    continue_id: 0,
                })
            }
        }
    }

    // Figure out which branch loops and which branch should merge, also find any potential break ops.
    for (bi, looping_branch_idx) in branch_conditional_ops {
        let merge_branch_idx = (looping_branch_idx + 1) % 2;
        let id = &blocks[bi].label_id().unwrap();
        let out = outgoing_edges(&blocks[bi]);

        let continue_block_id = eliminate_multiple_continue_blocks(blocks, *id);
        let merge_block_id = out[merge_branch_idx];

        if cf_info.used(continue_block_id) {
            let new_id = split_block(header, blocks, continue_block_id);
            cf_info.retarget(continue_block_id, new_id);
        }
        if cf_info.used(merge_block_id) {
            let new_id = split_block(header, blocks, merge_block_id);
            cf_info.retarget(merge_block_id, new_id);
        }

        let bi = find_block_index_from_id(blocks, id); // after this we don't insert or remove blocks
        let check_block = &mut blocks[bi];

        let merge_operands = vec![
            Operand::IdRef(merge_block_id),
            Operand::IdRef(continue_block_id),
            Operand::SelectionControl(SelectionControl::NONE),
        ];

        cf_info.set_loops_continue_and_merge(*id, merge_block_id, continue_block_id);

        // Insert the merge instruction
        let merge_inst = Instruction::new(Op::LoopMerge, None, None, merge_operands);
        check_block
            .instructions
            .insert(check_block.instructions.len() - 1, merge_inst);
    }
}
