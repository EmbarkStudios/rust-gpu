// This pass inserts merge instructions for structured control flow with the assumption the spir-v is reducible.

use super::simple_passes::outgoing_edges;
use crate::decorations::UnrollLoopsDecoration;
use rspirv::spirv::{Op, SelectionControl, Word};
use rspirv::{
    dr::{Block, Builder, InsertPoint, Module, Operand},
    spirv::LoopControl,
};
use rustc_data_structures::fx::FxHashMap;
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

    fn set_names(&self, builder: &mut Builder) {
        for loop_info in &self.loops {
            builder.name(loop_info.header_id, "loop_header".to_string());
            builder.name(loop_info.merge_id, "loop_merge".to_string());
            builder.name(loop_info.continue_id, "loop_continue".to_string());
        }
        for id in &self.if_merge_ids {
            builder.name(*id, "if_merge".to_string());
        }
        for id in &self.switch_merge_ids {
            builder.name(*id, "switch_merge".to_string());
        }
    }
}

pub fn structurize(
    sess: &Session,
    module: Module,
    unroll_loops_decorations: FxHashMap<Word, UnrollLoopsDecoration>,
) -> Module {
    let mut builder = Builder::new_from_module(module);

    for func_idx in 0..builder.module_ref().functions.len() {
        let mut cf_info = ControlFlowInfo::new();

        builder.select_function(Some(func_idx)).unwrap();

        let func_id = builder.module_ref().functions[func_idx]
            .def
            .as_ref()
            .unwrap()
            .result_id
            .unwrap();

        let loop_control = match unroll_loops_decorations.get(&func_id) {
            Some(UnrollLoopsDecoration {}) => LoopControl::UNROLL,
            None => LoopControl::NONE,
        };

        insert_loop_merge_on_conditional_branch(&mut builder, &mut cf_info, loop_control);
        retarget_loop_children_if_needed(&mut builder, &cf_info);
        insert_selection_merge_on_conditional_branch(sess, &mut builder, &mut cf_info);
        defer_loop_internals(&mut builder, &cf_info);
        cf_info.set_names(&mut builder);
    }

    builder.module()
}

fn get_blocks_mut(builder: &mut Builder) -> &mut Vec<Block> {
    let function = builder.selected_function().unwrap();
    &mut builder.module_mut().functions[function].blocks
}

fn get_blocks_ref(builder: &Builder) -> &[Block] {
    let function = builder.selected_function().unwrap();
    &builder.module_ref().functions[function].blocks
}

fn find_block_index_from_id(builder: &Builder, id: &Word) -> usize {
    for (i, block) in get_blocks_ref(builder).iter().enumerate() {
        if block.label_id() == Some(*id) {
            return i;
        }
    }

    panic!("Failed to find block from id {}", id);
}

macro_rules! get_block_mut {
    ($builder:expr, $idx:expr) => {
        &mut get_blocks_mut($builder)[$idx]
    };
}

macro_rules! get_block_ref {
    ($builder:expr, $idx:expr) => {
        &get_blocks_ref($builder)[$idx]
    };
}

fn idx_to_id(builder: &mut Builder, idx: usize) -> Word {
    get_blocks_ref(builder)[idx].label_id().unwrap()
}

// some times break will yeet themselfs out of a parent loop by skipping the merge block. This prevents that.
fn retarget_loop_children_if_needed(builder: &mut Builder, cf_info: &ControlFlowInfo) {
    for loop_info in &cf_info.loops {
        let LoopInfo {
            header_id: header,
            merge_id: merge,
            ..
        } = loop_info;

        let mut next: VecDeque<Word> = VecDeque::new();
        next.push_back(*header);

        while let Some(front) = next.pop_front() {
            let block_idx = find_block_index_from_id(builder, &front);
            let mut new_edges =
                outgoing_edges(get_block_ref!(builder, block_idx)).collect::<Vec<_>>();

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
                if block_is_parent_of(builder, *merge, new_edges[0]) {
                    // retarget front to branch to merge.
                    let front_block = get_block_mut!(builder, block_idx);
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

fn incoming_edges(id: Word, builder: &mut Builder) -> Vec<Word> {
    let mut incoming_edges = Vec::new();
    for block in get_blocks_ref(builder) {
        if outgoing_edges(block).any(|x| x == id) {
            incoming_edges.push(block.label_id().unwrap());
        }
    }

    incoming_edges
}

fn num_incoming_edges(id: Word, builder: &mut Builder) -> usize {
    incoming_edges(id, builder).len()
}

// Turn a block into a conditional branch that either goes to yes or goes to merge.
fn change_block_to_switch(
    builder: &mut Builder,
    block_id: Word,
    cases: &[Word],
    merge: Word,
    condition: Word,
) {
    let cb_idx = find_block_index_from_id(builder, &block_id);
    builder.select_block(Some(cb_idx)).unwrap();

    let target: Vec<(Operand, Word)> = cases
        .iter()
        .enumerate()
        .map(|(i, id)| (Operand::LiteralInt32(i as u32 + 1), *id))
        .collect();

    builder.pop_instruction().unwrap();
    builder
        .selection_merge(merge, SelectionControl::NONE)
        .unwrap();
    builder.switch(condition, merge, target).unwrap();
}

// detect the intermediate break block by checking whether a block that branches to a merge block has 2 parents.
fn defer_loop_internals(builder: &mut Builder, cf_info: &ControlFlowInfo) {
    for loop_info in &cf_info.loops {
        // find all blocks that branch to a merge block.
        let mut possible_intermediate_block_idexes = Vec::new();
        for (i, block) in get_blocks_ref(builder).iter().enumerate() {
            let mut out = outgoing_edges(block);
            if out.next() == Some(loop_info.merge_id) && out.next() == None {
                possible_intermediate_block_idexes.push(i)
            }
        }
        // check how many incoming edges the branch has and use that to collect a list of intermediate blocks.
        let mut intermediate_block_ids = Vec::new();
        for i in possible_intermediate_block_idexes {
            let intermediate_block_id = idx_to_id(builder, i);
            let num_incoming_edges = num_incoming_edges(intermediate_block_id, builder);
            if num_incoming_edges > 1 {
                intermediate_block_ids.push(intermediate_block_id);
            }
        }

        if !intermediate_block_ids.is_empty() {
            // Create a new empty block.
            let old_merge_block_id = split_block(builder, loop_info.merge_id, false);

            // Create Phi
            let phi_result_id = builder.id();
            let int_type_id = builder.type_int(32, 1);
            let const_0_id = builder.constant_u32(int_type_id, 0);

            let mut phi_operands = vec![];
            for (intermediate_i, intermediate_block_id) in intermediate_block_ids.iter().enumerate()
            {
                let intermediate_i = intermediate_i as u32 + 1;
                let const_x_id = builder.constant_u32(int_type_id, intermediate_i);
                let t = incoming_edges(*intermediate_block_id, builder);
                for blocks_that_go_to_intermediate in t {
                    phi_operands.push((const_x_id, blocks_that_go_to_intermediate));
                }
                builder.name(*intermediate_block_id, "deferred".to_string());
            }
            phi_operands.push((const_0_id, loop_info.header_id));

            builder
                .select_block(Some(find_block_index_from_id(builder, &loop_info.merge_id)))
                .unwrap();
            builder
                .insert_phi(
                    InsertPoint::Begin,
                    int_type_id,
                    Some(phi_result_id),
                    phi_operands,
                )
                .unwrap();

            // point all intermediate blocks to the new empty merge block.
            for intermediate_block_id in intermediate_block_ids.iter() {
                for incoming_id in incoming_edges(*intermediate_block_id, builder) {
                    let incoming_idx = find_block_index_from_id(builder, &incoming_id);
                    let incoming_block = get_block_mut!(builder, incoming_idx);

                    for operand in &mut incoming_block.instructions.last_mut().unwrap().operands {
                        if *operand == Operand::IdRef(*intermediate_block_id) {
                            *operand = Operand::IdRef(loop_info.merge_id); // loop_info.merge_id is the same block as the new empty block from the last step.
                        }
                    }
                }
            }

            // Create a switch statement of all intermediate blocks.
            change_block_to_switch(
                builder,
                loop_info.merge_id,
                &intermediate_block_ids,
                old_merge_block_id,
                phi_result_id,
            );

            // point intermediate blocks to the old merge block.
            for intermediate_block_id in intermediate_block_ids.iter() {
                let intermediate_block_idx =
                    find_block_index_from_id(builder, intermediate_block_id);
                for operand in &mut get_block_mut!(builder, intermediate_block_idx)
                    .instructions
                    .last_mut()
                    .unwrap()
                    .operands
                {
                    if *operand == Operand::IdRef(loop_info.merge_id) {
                        *operand = Operand::IdRef(old_merge_block_id);
                    }
                }
            }
        }
    }
}

// "Combines" all continue blocks into 1 and returns the ID of the continue block.
fn eliminate_multiple_continue_blocks(builder: &mut Builder, header: Word) -> Word {
    // Find all possible continue blocks.
    let mut continue_blocks = Vec::new();
    for block in get_blocks_ref(builder) {
        let block_id = block.label_id().unwrap();
        if ends_in_branch(block) {
            let edge = outgoing_edges(block).next().unwrap();
            if edge == header && block_is_parent_of(builder, header, block_id) {
                continue_blocks.push(block_id);
            }
        }
    }
    // if there are multiple continue blocks we need to retarget towards a single continue.
    if continue_blocks.len() > 1 {
        let continue_block_id = continue_blocks.last().unwrap();
        for block_id in continue_blocks.iter().take(continue_blocks.len() - 1) {
            let idx = find_block_index_from_id(builder, block_id);
            let block = get_block_mut!(builder, idx);
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

fn block_leads_into_break(builder: &Builder, cf_info: &ControlFlowInfo, start: Word) -> bool {
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(start);

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(builder, &front);
        let mut new_edges = outgoing_edges(get_block_ref!(builder, block_idx)).collect::<Vec<_>>();

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
        if ends_in_branch_conditional(get_block_ref!(builder, block_idx)) {
            new_edges.clear();
        }

        // if front is a merge block return true
        for loop_info in &cf_info.loops {
            if front == loop_info.merge_id
                && block_is_parent_of(builder, loop_info.header_id, start)
            {
                return true;
            }
        }

        next.extend(new_edges);
    }

    false
}

fn block_leads_into_continue(builder: &Builder, cf_info: &ControlFlowInfo, start: Word) -> bool {
    let start_idx = find_block_index_from_id(builder, &start);
    let new_edges = outgoing_edges(get_block_ref!(builder, start_idx)).collect::<Vec<_>>();
    for loop_info in &cf_info.loops {
        if new_edges.len() == 1 && loop_info.continue_id == new_edges[0] {
            return true;
        }
    }

    false
}

// every branch from a reaches b.
fn block_is_reverse_idom_of(
    builder: &Builder,
    cf_info: &ControlFlowInfo,
    a: Word,
    b: Word,
) -> bool {
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(a);

    let mut processed = vec![a]; // ensures we are not looping.

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(builder, &front);

        if front == b {
            continue;
        }

        let mut new_edges = outgoing_edges(get_block_ref!(builder, block_idx)).collect::<Vec<_>>();

        // Skip loop bodies by jumping to the merge block is we hit a header block.
        for loop_info in &cf_info.loops {
            if front == loop_info.header_id {
                // TODO: should only do this for children i guess.
                new_edges = vec![loop_info.merge_id];
            }
        }

        for loop_info in &cf_info.loops {
            // Make sure we are not looping.
            if block_is_parent_of(builder, loop_info.header_id, a)
                && new_edges.contains(&loop_info.header_id)
            {
                let index = new_edges
                    .iter()
                    .position(|x| *x == loop_info.header_id)
                    .unwrap();
                new_edges.remove(index);
            }

            // Make sure we are not continuing after a merge.
            if block_is_parent_of(builder, loop_info.header_id, a) && front == loop_info.merge_id {
                new_edges.clear();
            }
        }

        if new_edges.is_empty() {
            return false;
        }

        for id in &processed {
            if let Some(i) = new_edges.iter().position(|x| x == id) {
                new_edges.remove(i);
            }
        }

        processed.push(front);
        next.extend(new_edges);
    }

    true
}
fn get_possible_merge_positions(
    builder: &Builder,
    cf_info: &ControlFlowInfo,
    start: Word,
) -> Vec<usize> {
    let mut retval = Vec::new();
    for (idx, block) in get_blocks_ref(builder).iter().enumerate() {
        if block_is_reverse_idom_of(builder, cf_info, start, block.label_id().unwrap()) {
            retval.push(idx);
        }
    }

    retval
}

fn block_is_parent_of(builder: &Builder, parent: Word, child: Word) -> bool {
    let mut next: VecDeque<Word> = VecDeque::new();
    next.push_back(parent);

    let mut processed = vec![parent]; // ensures we are not looping.

    while let Some(front) = next.pop_front() {
        let block_idx = find_block_index_from_id(builder, &front);
        let mut new_edges = outgoing_edges(get_block_ref!(builder, block_idx)).collect::<Vec<_>>();

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
    builder: &Builder,
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

        let block_idx = find_block_index_from_id(builder, &front);
        let mut new_edges = outgoing_edges(get_block_ref!(builder, block_idx)).collect::<Vec<_>>();

        let edge_it = new_edges.iter().find(|&x| x == &start); // Check if the new_edges contain the start
        if new_edges.len() == 1 {
            if let Some(edge_it) = edge_it {
                // loop over the orginal edges to find which branch is looping
                let start_idx = find_block_index_from_id(builder, &front);
                let start_edges = outgoing_edges(get_block_ref!(builder, start_idx));

                for (i, start_edge) in start_edges.enumerate() {
                    if start_edge == *edge_it || block_is_parent_of(builder, start_edge, *edge_it) {
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
fn split_block(builder: &mut Builder, block_to_split: Word, retarget: bool) -> Word {
    // assign old block new id.
    let new_original_block_id = builder.id();
    let block_to_split_index = find_block_index_from_id(builder, &block_to_split);
    let orignial_block = get_block_mut!(builder, block_to_split_index);
    orignial_block.label.as_mut().unwrap().result_id = Some(new_original_block_id);
    // create new block with old id.
    builder.begin_block(Some(block_to_split)).unwrap();
    // new block branches to old block.
    builder.branch(new_original_block_id).unwrap();
    if retarget {
        // update all merge ops to point the the old block with its new id.
        for block in get_blocks_mut(builder) {
            for inst in &mut block.instructions {
                if inst.class.opcode == Op::LoopMerge || inst.class.opcode == Op::SelectionMerge {
                    for operand in &mut inst.operands {
                        if *operand == Operand::IdRef(block_to_split) {
                            *operand = Operand::IdRef(new_original_block_id);
                        }
                    }
                }
            }
        }
    }

    new_original_block_id
}

fn make_unreachable_block(builder: &mut Builder) -> Word {
    let id = builder.id();
    builder.begin_block(Some(id)).unwrap();
    builder.unreachable().unwrap();
    id
}

pub fn insert_selection_merge_on_conditional_branch(
    sess: &Session,
    builder: &mut Builder,
    cf_info: &mut ControlFlowInfo,
) {
    let mut branch_conditional_ops = Vec::new();

    // Find conditional branches that are not loops
    for block in get_blocks_ref(builder) {
        if ends_in_branch_conditional(block)
            && !cf_info.id_is_loops_header(block.label_id().unwrap())
        {
            branch_conditional_ops.push(block.label_id().unwrap());
        }
    }

    let mut modified_ids = FxHashMap::default();

    // Find convergence point.
    for id in branch_conditional_ops.iter() {
        let id = match modified_ids.get_key_value(id) {
            Some((_, value)) => value,
            None => id,
        };

        let bi = find_block_index_from_id(builder, id);
        let out = outgoing_edges(&get_blocks_ref(builder)[bi]).collect::<Vec<_>>();
        let id = idx_to_id(builder, bi);
        let a_nexts = get_possible_merge_positions(builder, cf_info, out[0]);
        let b_nexts = get_possible_merge_positions(builder, cf_info, out[1]);

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
            idx_to_id(builder, idx)
        } else {
            let a_first_id = out[0];
            let b_first_id = out[1];
            let a_last_idx = match a_nexts.last() {
                Some(last) => *last,
                None => find_block_index_from_id(builder, &out[0]),
            };
            let b_last_idx = match b_nexts.last() {
                Some(last) => *last,
                None => find_block_index_from_id(builder, &out[1]),
            };

            let branch_a_breaks = block_leads_into_break(builder, cf_info, a_first_id);
            let branch_b_breaks = block_leads_into_break(builder, cf_info, b_first_id);
            let branch_a_continues = block_leads_into_continue(builder, cf_info, a_first_id);
            let branch_b_continues = block_leads_into_continue(builder, cf_info, b_first_id);
            let branch_a_returns = ends_in_return(get_block_ref!(builder, a_last_idx));
            let branch_b_returns = ends_in_return(get_block_ref!(builder, b_last_idx));

            if ((branch_a_breaks || branch_a_continues) && (branch_b_breaks || branch_b_continues))
                || branch_a_returns && branch_b_returns
            {
                // (fully unreachable) insert a rando block and mark as merge.
                make_unreachable_block(builder)
            } else if branch_a_breaks || branch_a_continues || branch_a_returns {
                // (partially unreachable) merge block becomes branch b immediatly
                b_first_id
            } else if branch_b_breaks || branch_b_continues || branch_b_returns {
                // (partially unreachable) merge block becomes branch a immediatly
                a_first_id
            } else {
                // In theory this should never happen.
                sess.fatal("UNEXPECTED, Unknown exit detected.");
            }
        };

        if cf_info.used(merge_block_id) {
            let new_id = split_block(builder, merge_block_id, true);
            cf_info.retarget(merge_block_id, new_id);

            if branch_conditional_ops.contains(&merge_block_id) {
                modified_ids.insert(merge_block_id, new_id);
            }
        }

        cf_info.if_merge_ids.push(merge_block_id);

        // Insert the merge instruction
        let bi = find_block_index_from_id(builder, &id); // after this we don't insert or remove blocks
        builder.select_block(Some(bi)).unwrap();
        builder
            .insert_selection_merge(
                InsertPoint::FromEnd(1),
                merge_block_id,
                SelectionControl::NONE,
            )
            .unwrap();
    }
}

pub fn insert_loop_merge_on_conditional_branch(
    builder: &mut Builder,
    cf_info: &mut ControlFlowInfo,
    loop_control: LoopControl,
) {
    let mut branch_conditional_ops = Vec::new();

    // Find conditional branches that are loops, and find which branch is the one that loops.
    for block in get_blocks_ref(builder) {
        if ends_in_branch_conditional(block) {
            let block_id = block.label_id().unwrap();
            if let Some(looping_branch_idx) =
                get_looping_branch_from_block(builder, cf_info, block_id)
            {
                branch_conditional_ops.push((block_id, looping_branch_idx));
                cf_info.loops.push(LoopInfo {
                    header_id: block_id,
                    merge_id: 0,
                    continue_id: 0,
                })
            }
        }
    }
    let mut modified_ids = FxHashMap::default();

    // Figure out which branch loops and which branch should merge, also find any potential break ops.
    for (id, looping_branch_idx) in branch_conditional_ops.iter() {
        let id = match modified_ids.get_key_value(id) {
            Some((_, value)) => *value,
            None => *id,
        };

        let merge_branch_idx = (looping_branch_idx + 1) % 2;
        let bi = find_block_index_from_id(builder, &id);
        let out = outgoing_edges(&get_blocks_ref(builder)[bi]).collect::<Vec<_>>();

        let continue_block_id = eliminate_multiple_continue_blocks(builder, id);
        let merge_block_id = out[merge_branch_idx];

        if cf_info.used(continue_block_id) {
            let new_id = split_block(builder, continue_block_id, true);
            cf_info.retarget(continue_block_id, new_id);

            if branch_conditional_ops.contains(&(continue_block_id, *looping_branch_idx)) {
                modified_ids.insert(continue_block_id, new_id);
            }
        }
        if cf_info.used(merge_block_id) {
            let new_id = split_block(builder, merge_block_id, true);
            cf_info.retarget(merge_block_id, new_id);

            if branch_conditional_ops.contains(&(merge_block_id, *looping_branch_idx)) {
                modified_ids.insert(merge_block_id, new_id);
            }
        }

        cf_info.set_loops_continue_and_merge(id, merge_block_id, continue_block_id);

        // Insert the merge instruction
        let bi = find_block_index_from_id(builder, &id); // after this we don't insert or remove blocks
        builder.select_block(Some(bi)).unwrap();
        builder
            .insert_loop_merge(
                InsertPoint::FromEnd(1),
                merge_block_id,
                continue_block_id,
                loop_control,
                None,
            )
            .unwrap();
    }
}
