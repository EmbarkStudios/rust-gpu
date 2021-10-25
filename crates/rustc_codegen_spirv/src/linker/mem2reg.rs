//! This algorithm is not intended to be an optimization, it is rather for legalization.
//! Specifically, spir-v disallows things like a `StorageClass::Function` pointer to a
//! `StorageClass::Input` pointer. Our frontend definitely allows it, though, this is like taking a
//! `&Input<T>` in a function! So, we inline all functions (see inline.rs) that take these
//! "illegal" pointers, then run mem2reg on the result to "unwrap" the Function pointer.
//!
//! Because it's merely a legalization pass, this computes "minimal" SSA form, *not* "pruned" SSA
//! form. The difference is that "minimal" may include extra phi nodes that aren't actually used
//! anywhere - we assume that later optimization passes will take care of these (relying on what
//! wikipedia calls "treat pruning as a dead code elimination problem").

use super::simple_passes::outgoing_edges;
use super::{apply_rewrite_rules, id};
use rspirv::dr::{Block, Function, Instruction, ModuleHeader, Operand};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_middle::bug;
use std::collections::hash_map;

pub fn mem2reg(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    pointer_to_pointee: &FxHashMap<Word, Word>,
    constants: &FxHashMap<Word, u32>,
    func: &mut Function,
) {
    let reachable = compute_reachable(&func.blocks);
    let preds = compute_preds(&func.blocks, &reachable);
    let idom = compute_idom(&preds, &reachable);
    let dominance_frontier = compute_dominance_frontier(&preds, &idom);
    loop {
        let changed = insert_phis_all(
            header,
            types_global_values,
            pointer_to_pointee,
            constants,
            &mut func.blocks,
            &dominance_frontier,
        );
        if !changed {
            break;
        }
        // mem2reg produces minimal SSA form, not pruned, so DCE the dead ones
        super::dce::dce_phi(func);
    }
}

fn label_to_index(blocks: &[Block], id: Word) -> usize {
    blocks
        .iter()
        .position(|b| b.label_id().unwrap() == id)
        .unwrap()
}

fn compute_reachable(blocks: &[Block]) -> Vec<bool> {
    fn recurse(blocks: &[Block], reachable: &mut [bool], block: usize) {
        if !reachable[block] {
            reachable[block] = true;
            for dest_id in outgoing_edges(&blocks[block]) {
                let dest_idx = label_to_index(blocks, dest_id);
                recurse(blocks, reachable, dest_idx);
            }
        }
    }
    let mut reachable = vec![false; blocks.len()];
    recurse(blocks, &mut reachable, 0);
    reachable
}

fn compute_preds(blocks: &[Block], reachable_blocks: &[bool]) -> Vec<Vec<usize>> {
    let mut result = vec![vec![]; blocks.len()];
    // Do not count unreachable blocks as valid preds of blocks
    for (source_idx, source) in blocks
        .iter()
        .enumerate()
        .filter(|&(b, _)| reachable_blocks[b])
    {
        for dest_id in outgoing_edges(source) {
            let dest_idx = label_to_index(blocks, dest_id);
            result[dest_idx].push(source_idx);
        }
    }
    result
}

// Paper: A Simple, Fast Dominance Algorithm
// https://www.cs.rice.edu/~keith/EMBED/dom.pdf
// Note: requires nodes in reverse postorder
// If a result is None, that means the block is unreachable, and therefore has no idom.
fn compute_idom(preds: &[Vec<usize>], reachable_blocks: &[bool]) -> Vec<Option<usize>> {
    fn intersect(doms: &[Option<usize>], mut finger1: usize, mut finger2: usize) -> usize {
        // TODO: This may return an optional result?
        while finger1 != finger2 {
            // Note: The comparisons here are inverted from the paper, because the paper uses
            // comparison to be postorder index. However, we have reverse postorder indices.
            while finger1 > finger2 {
                finger1 = doms[finger1].unwrap();
            }
            while finger2 > finger1 {
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
        // Unreachable blocks have no preds, and therefore no idom
        for node in (1..(preds.len())).filter(|&i| reachable_blocks[i]) {
            let mut new_idom: Option<usize> = None;
            for &pred in &preds[node] {
                if idom[pred].is_some() {
                    new_idom =
                        Some(new_idom.map_or(pred, |new_idom| intersect(&idom, pred, new_idom)));
                }
            }
            // TODO: This may return an optional result?
            let new_idom = new_idom.unwrap();
            if idom[node] != Some(new_idom) {
                idom[node] = Some(new_idom);
                changed = true;
            }
        }
    }
    assert!(idom
        .iter()
        .enumerate()
        .all(|(i, x)| x.is_some() == reachable_blocks[i]));
    idom
}

// Same paper as above
fn compute_dominance_frontier(
    preds: &[Vec<usize>],
    idom: &[Option<usize>],
) -> Vec<FxHashSet<usize>> {
    assert_eq!(preds.len(), idom.len());
    let mut dominance_frontier = vec![FxHashSet::default(); preds.len()];
    for node in 0..preds.len() {
        if preds[node].len() >= 2 {
            let node_idom = idom[node].unwrap();
            for &pred in &preds[node] {
                let mut runner = pred;
                while runner != node_idom {
                    dominance_frontier[runner].insert(node);
                    runner = idom[runner].unwrap();
                }
            }
        }
    }
    dominance_frontier
}

// Returns true if variables were rewritten
fn insert_phis_all(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    pointer_to_pointee: &FxHashMap<Word, Word>,
    constants: &FxHashMap<Word, u32>,
    blocks: &mut [Block],
    dominance_frontier: &[FxHashSet<usize>],
) -> bool {
    let var_maps_and_types = blocks[0]
        .instructions
        .iter()
        .filter(|inst| inst.class.opcode == Op::Variable)
        .filter_map(|inst| {
            let var = inst.result_id.unwrap();
            let var_ty = *pointer_to_pointee.get(&inst.result_type.unwrap()).unwrap();
            Some((
                collect_access_chains(pointer_to_pointee, constants, blocks, var, var_ty)?,
                var_ty,
            ))
        })
        .collect::<Vec<_>>();
    if var_maps_and_types.is_empty() {
        return false;
    }
    for (var_map, _) in &var_maps_and_types {
        split_copy_memory(header, blocks, var_map);
    }
    for &(ref var_map, base_var_type) in &var_maps_and_types {
        let blocks_with_phi = insert_phis(blocks, dominance_frontier, var_map);
        let mut renamer = Renamer {
            header,
            types_global_values,
            blocks,
            blocks_with_phi,
            base_var_type,
            var_map,
            phi_defs: FxHashSet::default(),
            visited: FxHashSet::default(),
            stack: Vec::new(),
            rewrite_rules: FxHashMap::default(),
        };
        renamer.rename(0, None);
        apply_rewrite_rules(&renamer.rewrite_rules, blocks);
        remove_nops(blocks);
    }
    remove_old_variables(blocks, &var_maps_and_types);
    true
}

#[derive(Debug)]
struct VarInfo {
    // Type of the *dereferenced* variable.
    ty: Word,
    // OpAccessChain indexes off the base variable
    indices: Vec<u32>,
}

fn collect_access_chains(
    pointer_to_pointee: &FxHashMap<Word, Word>,
    constants: &FxHashMap<Word, u32>,
    blocks: &[Block],
    base_var: Word,
    base_var_ty: Word,
) -> Option<FxHashMap<Word, VarInfo>> {
    fn construct_access_chain_info(
        pointer_to_pointee: &FxHashMap<Word, Word>,
        constants: &FxHashMap<Word, u32>,
        inst: &Instruction,
        base: &VarInfo,
    ) -> Option<VarInfo> {
        Some(VarInfo {
            ty: *pointer_to_pointee.get(&inst.result_type.unwrap()).unwrap(),
            indices: {
                let mut base_indicies = base.indices.clone();
                for op in inst.operands.iter().skip(1) {
                    base_indicies.push(*constants.get(&op.id_ref_any().unwrap())?);
                }
                base_indicies
            },
        })
    }

    let mut variables = FxHashMap::default();
    variables.insert(
        base_var,
        VarInfo {
            ty: base_var_ty,
            indices: vec![],
        },
    );
    // Loop in case a previous block references a later AccessChain
    loop {
        let mut changed = false;
        for inst in blocks.iter().flat_map(|b| &b.instructions) {
            for (index, op) in inst.operands.iter().enumerate() {
                if let Operand::IdRef(id) = op {
                    if variables.contains_key(id) {
                        match inst.class.opcode {
                            // Only allow store if pointer is the lhs, not rhs
                            Op::Store if index == 0 => {}
                            Op::Load
                            | Op::AccessChain
                            | Op::InBoundsAccessChain
                            | Op::CopyMemory => {}
                            _ => return None,
                        }
                    }
                }
            }
            if let Op::AccessChain | Op::InBoundsAccessChain = inst.class.opcode {
                if let Some(base) = variables.get(&inst.operands[0].id_ref_any().unwrap()) {
                    let info =
                        construct_access_chain_info(pointer_to_pointee, constants, inst, base)?;
                    match variables.entry(inst.result_id.unwrap()) {
                        hash_map::Entry::Vacant(entry) => {
                            entry.insert(info);
                            changed = true;
                        }
                        hash_map::Entry::Occupied(_) => {}
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    Some(variables)
}

// Splits an OpCopyMemory into an OpLoad followed by an OpStore. This is because we want to be able
// to mem2reg variables used in OpCopyMemory, but analysis becomes very difficult: we only analyze
// one variable at a time, but OpCopyMemory can copy between two local variables (both of which are
// getting mem2reg'd), requiring cross-analysis shenanigans. So, if we know at least one side of
// the OpCopyMemory is getting mem2reg'd, we can safely split it into a load/store pair: at least
// one side of the pair is going to evaporate in the subsequent rewrite. Then, we can only deal
// with one side of a pair at a time, treating the other side as opaque (but possibly rewriting
// both sides).
//
// This means that an OpCopyMemory between two local variables will completely disappear, while an
// OpCopyMemory from a global to a local will turn into an OpLoad, and local to global will turn
// into an OpStore.
//
// Note that while we only look at a single var map in this function, if an OpCopyMemory contains
// variables from two var maps, the second pass won't do anything since the first pass will already
// have split it (but that's fine, it would have done the same thing anyway).
//
// Finally, an edge case to keep in mind is that an OpCopyMemory can happen between two vars in the
// same var map (e.g. `s.x = s.y;`).
fn split_copy_memory(
    header: &mut ModuleHeader,
    blocks: &mut [Block],
    var_map: &FxHashMap<Word, VarInfo>,
) {
    for block in blocks {
        let mut inst_index = 0;
        while inst_index < block.instructions.len() {
            let inst = &block.instructions[inst_index];
            if inst.class.opcode == Op::CopyMemory {
                let target = inst.operands[0].id_ref_any().unwrap();
                let source = inst.operands[1].id_ref_any().unwrap();
                if inst.operands.len() > 2 {
                    // TODO: Copy the memory operands to the load/store
                    bug!("mem2reg OpCopyMemory doesn't support memory operands yet");
                }
                let ty = match (var_map.get(&target), var_map.get(&source)) {
                    (None, None) => {
                        inst_index += 1;
                        continue;
                    }
                    (Some(target), None) => target.ty,
                    (None, Some(source)) => source.ty,
                    (Some(target), Some(source)) => {
                        assert_eq!(target.ty, source.ty);
                        target.ty
                    }
                };
                let temp_id = id(header);
                block.instructions[inst_index] = Instruction::new(
                    Op::Load,
                    Some(ty),
                    Some(temp_id),
                    vec![Operand::IdRef(source)],
                );
                inst_index += 1;
                block.instructions.insert(
                    inst_index,
                    Instruction::new(
                        Op::Store,
                        None,
                        None,
                        vec![Operand::IdRef(target), Operand::IdRef(temp_id)],
                    ),
                );
            }
            inst_index += 1;
        }
    }
}

fn has_store(block: &Block, var_map: &FxHashMap<Word, VarInfo>) -> bool {
    block.instructions.iter().any(|inst| {
        let ptr = match inst.class.opcode {
            Op::Store => inst.operands[0].id_ref_any().unwrap(),
            Op::Variable if inst.operands.len() < 2 => return false,
            Op::Variable => inst.result_id.unwrap(),
            _ => return false,
        };
        var_map.contains_key(&ptr)
    })
}

fn insert_phis(
    blocks: &[Block],
    dominance_frontier: &[FxHashSet<usize>],
    var_map: &FxHashMap<Word, VarInfo>,
) -> FxHashSet<usize> {
    // TODO: Some algorithms check if the var is trivial in some way, e.g. all loads and stores are
    // in a single block. We should probably do that too.
    let mut ever_on_work_list = FxHashSet::default();
    let mut work_list = Vec::new();
    let mut blocks_with_phi = FxHashSet::default();
    for (block_idx, block) in blocks.iter().enumerate() {
        if has_store(block, var_map) {
            ever_on_work_list.insert(block_idx);
            work_list.push(block_idx);
        }
    }
    while let Some(x) = work_list.pop() {
        for &y in &dominance_frontier[x] {
            if blocks_with_phi.insert(y) && ever_on_work_list.insert(y) {
                work_list.push(y);
            }
        }
    }
    blocks_with_phi
}

// These can't be part of the Renamer impl due to borrowck rules.
fn undef_for(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    ty: Word,
) -> Word {
    // TODO: This is horribly slow, fix this
    let existing = types_global_values
        .iter()
        .find(|inst| inst.class.opcode == Op::Undef && inst.result_type.unwrap() == ty);
    if let Some(existing) = existing {
        return existing.result_id.unwrap();
    }
    let inst_id = id(header);
    types_global_values.push(Instruction::new(Op::Undef, Some(ty), Some(inst_id), vec![]));
    inst_id
}
fn top_stack_or_undef(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    stack: &[Word],
    ty: Word,
) -> Word {
    match stack.last() {
        Some(&top) => top,
        None => undef_for(header, types_global_values, ty),
    }
}

struct Renamer<'a> {
    header: &'a mut ModuleHeader,
    types_global_values: &'a mut Vec<Instruction>,
    blocks: &'a mut [Block],
    blocks_with_phi: FxHashSet<usize>,
    base_var_type: Word,
    var_map: &'a FxHashMap<Word, VarInfo>,
    phi_defs: FxHashSet<Word>,
    visited: FxHashSet<usize>,
    stack: Vec<Word>,
    rewrite_rules: FxHashMap<Word, Word>,
}

impl Renamer<'_> {
    // Returns the phi definition.
    fn insert_phi_value(&mut self, block: usize, from_block: usize) -> Word {
        let from_block_label = self.blocks[from_block].label_id().unwrap();
        let phi_defs = &self.phi_defs;
        let existing_phi = self.blocks[block].instructions.iter_mut().find(|inst| {
            inst.class.opcode == Op::Phi && phi_defs.contains(&inst.result_id.unwrap())
        });
        let top_def = top_stack_or_undef(
            self.header,
            self.types_global_values,
            &self.stack,
            self.base_var_type,
        );
        match existing_phi {
            None => {
                let new_id = id(self.header);
                self.blocks[block].instructions.insert(
                    0,
                    Instruction::new(
                        Op::Phi,
                        Some(self.base_var_type),
                        Some(new_id),
                        vec![Operand::IdRef(top_def), Operand::IdRef(from_block_label)],
                    ),
                );
                self.phi_defs.insert(new_id);
                new_id
            }
            Some(existing_phi) => {
                existing_phi.operands.extend_from_slice(&[
                    Operand::IdRef(top_def),
                    Operand::IdRef(from_block_label),
                ]);
                existing_phi.result_id.unwrap()
            }
        }
    }

    fn rename(&mut self, block: usize, from_block: Option<usize>) {
        let original_stack = self.stack.len();

        if let Some(from_block) = from_block {
            if self.blocks_with_phi.contains(&block) {
                let new_top = self.insert_phi_value(block, from_block);
                self.stack.push(new_top);
            }
        }

        if !self.visited.insert(block) {
            while self.stack.len() > original_stack {
                self.stack.pop();
            }
            return;
        }

        for inst in &mut self.blocks[block].instructions {
            if inst.class.opcode == Op::Variable && inst.operands.len() > 1 {
                let ptr = inst.result_id.unwrap();
                let val = inst.operands[1].id_ref_any().unwrap();
                if let Some(var_info) = self.var_map.get(&ptr) {
                    assert_eq!(var_info.indices, Vec::<u32>::new());
                    self.stack.push(val);
                }
            } else if inst.class.opcode == Op::Store {
                let ptr = inst.operands[0].id_ref_any().unwrap();
                let val = inst.operands[1].id_ref_any().unwrap();
                if let Some(var_info) = self.var_map.get(&ptr) {
                    if var_info.indices.is_empty() {
                        *inst = Instruction::new(Op::Nop, None, None, vec![]);
                        self.stack.push(val);
                    } else {
                        let new_id = id(self.header);
                        let prev_comp = top_stack_or_undef(
                            self.header,
                            self.types_global_values,
                            &self.stack,
                            self.base_var_type,
                        );
                        let mut operands = vec![Operand::IdRef(val), Operand::IdRef(prev_comp)];
                        operands
                            .extend(var_info.indices.iter().copied().map(Operand::LiteralInt32));
                        *inst = Instruction::new(
                            Op::CompositeInsert,
                            Some(self.base_var_type),
                            Some(new_id),
                            operands,
                        );
                        self.stack.push(new_id);
                    }
                }
            } else if inst.class.opcode == Op::Load {
                let ptr = inst.operands[0].id_ref_any().unwrap();
                if let Some(var_info) = self.var_map.get(&ptr) {
                    let loaded_val = inst.result_id.unwrap();
                    // TODO: Should this do something more sane if it's undef?
                    let current_obj = top_stack_or_undef(
                        self.header,
                        self.types_global_values,
                        &self.stack,
                        self.base_var_type,
                    );
                    if var_info.indices.is_empty() {
                        *inst = Instruction::new(Op::Nop, None, None, vec![]);
                        self.rewrite_rules.insert(loaded_val, current_obj);
                    } else {
                        let new_id = id(self.header);
                        let mut operands = vec![Operand::IdRef(current_obj)];
                        operands
                            .extend(var_info.indices.iter().copied().map(Operand::LiteralInt32));
                        *inst = Instruction::new(
                            Op::CompositeExtract,
                            Some(var_info.ty),
                            Some(new_id),
                            operands,
                        );
                        self.rewrite_rules.insert(loaded_val, new_id);
                    }
                }
            }
        }

        for dest_id in outgoing_edges(&self.blocks[block]).collect::<Vec<_>>() {
            // TODO: Don't do this find
            let dest_idx = label_to_index(self.blocks, dest_id);
            self.rename(dest_idx, Some(block));
        }

        while self.stack.len() > original_stack {
            self.stack.pop();
        }
    }
}

fn remove_nops(blocks: &mut [Block]) {
    for block in blocks {
        block
            .instructions
            .retain(|inst| inst.class.opcode != Op::Nop);
    }
}

fn remove_old_variables(
    blocks: &mut [Block],
    var_maps_and_types: &[(FxHashMap<u32, VarInfo>, u32)],
) {
    blocks[0].instructions.retain(|inst| {
        inst.class.opcode != Op::Variable || {
            let result_id = inst.result_id.unwrap();
            var_maps_and_types
                .iter()
                .all(|(var_map, _)| !var_map.contains_key(&result_id))
        }
    });
    for block in blocks {
        block.instructions.retain(|inst| {
            !matches!(inst.class.opcode, Op::AccessChain | Op::InBoundsAccessChain)
                || inst.operands.iter().all(|op| {
                    op.id_ref_any().map_or(true, |id| {
                        var_maps_and_types
                            .iter()
                            .all(|(var_map, _)| !var_map.contains_key(&id))
                    })
                })
        });
    }
}
