use crate::simple_passes::outgoing_edges;
use crate::{apply_rewrite_rules, id, label_of, operand_idref};
use rspirv::dr::{Block, Function, Instruction, ModuleHeader, Operand};
use rspirv::spirv::{Op, Word};
use std::collections::HashMap;
use std::collections::HashSet;

pub fn mem2reg(
    header: &mut ModuleHeader,
    pointer_to_pointee: &HashMap<Word, Word>,
    func: &mut Function,
) {
    let preds = compute_preds(&func.blocks);
    let idom = compute_idom(&preds);
    let dominance_frontier = compute_dominance_frontier(&preds, &idom);
    insert_phis_all(
        header,
        pointer_to_pointee,
        &mut func.blocks,
        dominance_frontier,
    );
}

pub fn compute_preds(blocks: &[Block]) -> Vec<Vec<usize>> {
    let mut result = vec![vec![]; blocks.len()];
    for (source_idx, source) in blocks.iter().enumerate() {
        for dest_id in outgoing_edges(source) {
            let dest_idx = blocks.iter().position(|b| label_of(b) == dest_id).unwrap();
            result[dest_idx].push(source_idx);
        }
    }
    result
}

// Paper: A Simple, Fast Dominance Algorithm
// https://www.cs.rice.edu/~keith/EMBED/dom.pdf
// Note: requires nodes in reverse postorder
fn compute_idom(preds: &[Vec<usize>]) -> Vec<usize> {
    fn intersect(doms: &[Option<usize>], mut finger1: usize, mut finger2: usize) -> usize {
        // TODO: This may return an optional result?
        while finger1 != finger2 {
            while finger1 < finger2 {
                finger1 = doms[finger1].unwrap();
            }
            while finger2 < finger1 {
                finger2 = doms[finger2].unwrap();
            }
        }
        finger1
    }

    let mut idom = vec![None; preds.len()];
    idom[0] = Some(0);
    let mut changed = true;
    while changed {
        changed = false;
        for node in 1..(preds.len()) {
            let mut new_idom: Option<usize> = None;
            for &pred in &preds[node] {
                new_idom = Some(new_idom.map_or(pred, |new_idom| intersect(&idom, pred, new_idom)));
            }
            // TODO: This may return an optional result?
            let new_idom = new_idom.unwrap();
            if idom[node] != Some(new_idom) {
                idom[node] = Some(new_idom);
                changed = true;
            }
        }
    }
    idom.iter().map(|x| x.unwrap()).collect()
}

// Same paper as above
fn compute_dominance_frontier(preds: &[Vec<usize>], idom: &[usize]) -> Vec<HashSet<usize>> {
    assert_eq!(preds.len(), idom.len());
    let mut dominance_frontier = vec![HashSet::new(); preds.len()];
    for node in 0..preds.len() {
        if preds[node].len() >= 2 {
            for &pred in &preds[node] {
                let mut runner = pred;
                while runner != idom[node] {
                    dominance_frontier[runner].insert(node);
                    runner = idom[runner];
                }
            }
        }
    }
    dominance_frontier
}

fn insert_phis_all(
    header: &mut ModuleHeader,
    pointer_to_pointee: &HashMap<Word, Word>,
    blocks: &mut [Block],
    dominance_frontier: Vec<HashSet<usize>>,
) {
    let thing = blocks[0]
        .instructions
        .iter()
        .filter(|inst| inst.class.opcode == Op::Variable)
        .filter_map(|inst| {
            let var = inst.result_id.unwrap();
            if is_promotable(blocks, var) {
                let var_type = *pointer_to_pointee.get(&inst.result_type.unwrap()).unwrap();
                Some((var, var_type))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    for &(var, var_type) in &thing {
        insert_phis(header, blocks, &dominance_frontier, var, var_type);
    }
    blocks[0].instructions.retain(|inst| {
        inst.class.opcode != Op::Variable || {
            let result_id = inst.result_id.unwrap();
            thing.iter().all(|&(var, _)| var != result_id)
        }
    });
}

fn is_promotable(blocks: &[Block], var: Word) -> bool {
    for block in blocks {
        for inst in &block.instructions {
            for op in &inst.operands {
                if let Operand::IdRef(id) = *op {
                    if id == var {
                        match inst.class.opcode {
                            Op::Load | Op::Store => {}
                            _ => return false,
                        }
                    }
                }
            }
        }
    }
    true
}

// Returns the value for the definition.
fn find_last_store(block: &Block, var: Word) -> Option<Word> {
    block.instructions.iter().rev().find_map(|inst| {
        if inst.class.opcode == Op::Store && inst.operands[0] == Operand::IdRef(var)
            || inst.class.opcode == Op::Variable
                && inst.result_id == Some(var)
                && inst.operands.len() > 1
        {
            Some(operand_idref(&inst.operands[1]).unwrap())
        } else {
            None
        }
    })
}

fn insert_phis(
    header: &mut ModuleHeader,
    blocks: &mut [Block],
    dominance_frontier: &[HashSet<usize>],
    var: Word,
    var_type: Word,
) {
    // TODO: Some algorithms check if the var is trivial in some way, e.g. all loads and stores are
    // in a single block. We should probably do that too.
    let mut ever_on_work_list = HashSet::new();
    let mut work_list = Vec::new();
    let mut phi_defs = HashSet::new();
    for (block_idx, block) in blocks.iter().enumerate() {
        if let Some(def) = find_last_store(block, var) {
            ever_on_work_list.insert(block_idx);
            work_list.push((block_idx, def));
        }
    }
    while let Some((x, def)) = work_list.pop() {
        for &y in &dominance_frontier[x] {
            if let Some(new_def) = insert_phi(header, blocks, y, &mut phi_defs, var_type, x, def) {
                if ever_on_work_list.insert(y) {
                    work_list.push((y, new_def))
                }
            }
        }
    }

    let mut rewrite_rules = HashMap::new();
    rename(
        header,
        blocks,
        0,
        &phi_defs,
        var,
        &mut HashSet::new(),
        &mut Vec::new(),
        &mut rewrite_rules,
    );
    apply_rewrite_rules(&rewrite_rules, blocks);
    remove_nops(blocks);
}

// Returns the newly created phi definition.
fn insert_phi(
    header: &mut ModuleHeader,
    blocks: &mut [Block],
    block: usize,
    phi_defs: &mut HashSet<Word>,
    var_type: Word,
    from_block: usize,
    def: Word,
) -> Option<Word> {
    let from_block_label = label_of(&blocks[from_block]);
    let existing_phi = blocks[block]
        .instructions
        .iter_mut()
        .find(|inst| inst.class.opcode == Op::Phi && phi_defs.contains(&inst.result_id.unwrap()));
    match existing_phi {
        None => {
            let new_id = id(header);
            blocks[block].instructions.insert(
                0,
                Instruction::new(
                    Op::Phi,
                    Some(var_type),
                    Some(new_id),
                    vec![Operand::IdRef(def), Operand::IdRef(from_block_label)],
                ),
            );
            phi_defs.insert(new_id);
            Some(new_id)
        }
        Some(existing_phi) => {
            existing_phi
                .operands
                .extend_from_slice(&[Operand::IdRef(def), Operand::IdRef(from_block_label)]);
            None
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn rename(
    header: &mut ModuleHeader,
    blocks: &mut [Block],
    block: usize,
    phi_defs: &HashSet<Word>,
    var: Word,
    visited: &mut HashSet<usize>,
    stack: &mut Vec<Word>,
    rewrite_rules: &mut HashMap<Word, Word>,
) {
    if !visited.insert(block) {
        return;
    }

    let original_stack = stack.len();

    for inst in &mut blocks[block].instructions {
        if inst.class.opcode == Op::Phi {
            let result_id = inst.result_id.unwrap();
            if phi_defs.contains(&result_id) {
                stack.push(result_id);
            }
        } else if inst.class.opcode == Op::Variable && inst.operands.len() > 1 {
            let ptr = inst.result_id.unwrap();
            let val = operand_idref(&inst.operands[1]).unwrap();
            if ptr == var {
                stack.push(val);
            }
        } else if inst.class.opcode == Op::Store {
            let ptr = operand_idref(&inst.operands[0]).unwrap();
            let val = operand_idref(&inst.operands[1]).unwrap();
            if ptr == var {
                stack.push(val);
                *inst = Instruction::new(Op::Nop, None, None, vec![]);
            }
        } else if inst.class.opcode == Op::Load {
            let ptr = operand_idref(&inst.operands[0]).unwrap();
            let val = inst.result_id.unwrap();
            if ptr == var {
                rewrite_rules.insert(val, *stack.last().unwrap());
                *inst = Instruction::new(Op::Nop, None, None, vec![]);
            }
        }
    }

    for dest_id in outgoing_edges(&blocks[block]) {
        // TODO: Don't do this find
        let dest_idx = blocks.iter().position(|b| label_of(b) == dest_id).unwrap();
        rename(
            header,
            blocks,
            dest_idx,
            phi_defs,
            var,
            visited,
            stack,
            rewrite_rules,
        );
    }

    while stack.len() > original_stack {
        stack.pop();
    }
}

fn remove_nops(blocks: &mut [Block]) {
    for block in blocks {
        block
            .instructions
            .retain(|inst| inst.class.opcode != Op::Nop);
    }
}
