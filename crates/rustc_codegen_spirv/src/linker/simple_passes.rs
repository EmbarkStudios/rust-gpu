use bimap::BiHashMap;
use rspirv::dr::{Block, Function, Instruction, Module, Operand};
use rspirv::spirv::{Op, StorageClass, Word};
use std::collections::hash_map::Entry;
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
        // Reverse the order, so reverse-postorder keeps things tidy
        for &outgoing in outgoing_edges(current_block).iter().rev() {
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

pub fn outgoing_edges(block: &Block) -> Vec<Word> {
    let terminator = block.instructions.last().unwrap();
    // https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#Termination
    match terminator.class.opcode {
        Op::Branch => vec![terminator.operands[0].unwrap_id_ref()],
        Op::BranchConditional => vec![
            terminator.operands[1].unwrap_id_ref(),
            terminator.operands[2].unwrap_id_ref(),
        ],
        Op::Switch => once(terminator.operands[1].unwrap_id_ref())
            .chain(
                terminator.operands[3..]
                    .iter()
                    .step_by(2)
                    .map(|op| op.unwrap_id_ref()),
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

// OpAccessChain requires the result pointer storage class to match
// the storage class of the base.
// Modifies ops in place, but may add additional pointer types
// the old pointer types may then be unused.
// Additionally updates OpPhi return type to match the type of the
// access.
pub fn match_variable_storage_classes(module: &mut Module) {
    let mut ops = BiHashMap::<Word, (StorageClass, Word)>::new();

    // Get variables and pointers
    for inst in &module.types_global_values {
        match inst.class.opcode {
            Op::Variable | Op::TypePointer => {
                let result_type = match inst.class.opcode {
                    Op::Variable => inst.result_type.unwrap(),
                    Op::TypePointer => inst.operands[1].unwrap_id_ref(),
                    _ => unreachable!(),
                };
                let result_id = inst.result_id.unwrap();
                let storage_class = inst.operands[0].unwrap_storage_class();
                ops.insert(result_id, (storage_class, result_type));
            }
            _ => (),
        }
    }

    let header = module.header.as_mut().unwrap();
    let mut new_pointers = HashMap::<Word, Vec<Instruction>>::new();
    let mut changes = HashMap::<Word, Word>::new();

    for function in &mut module.functions {
        for inst in function.blocks.iter_mut().flat_map(|b| &mut b.instructions) {
            // TODO: Potentially handle PtrAccessChain?
            if let Op::AccessChain | Op::InBoundsAccessChain = inst.class.opcode {
                if let Some(&(base_storage_class, _)) =
                    ops.get_by_left(&inst.operands[0].unwrap_id_ref())
                {
                    let &(storage_class, pointee) =
                        ops.get_by_left(&inst.result_type.unwrap()).unwrap();
                    if storage_class != base_storage_class {
                        let pointer = if let Some(&pointer) =
                            ops.get_by_right(&(base_storage_class, pointee))
                        {
                            pointer
                        } else {
                            let pointer = super::id(header);
                            ops.insert_no_overwrite(pointer, (base_storage_class, pointee))
                                .unwrap();
                            let new_pointer = Instruction::new(
                                Op::TypePointer,
                                None,
                                Some(pointer),
                                vec![
                                    Operand::StorageClass(base_storage_class),
                                    Operand::IdRef(pointee),
                                ],
                            );
                            match new_pointers.entry(pointee) {
                                Entry::Occupied(mut occupied) => {
                                    occupied.get_mut().push(new_pointer);
                                }
                                Entry::Vacant(vacant) => {
                                    vacant.insert(vec![new_pointer]);
                                }
                            }
                            pointer
                        };
                        inst.result_type.replace(pointer);
                        ops.insert(inst.result_id.unwrap(), (base_storage_class, pointer));
                    }
                }
            }
        }

        for inst in function.blocks.iter_mut().flat_map(|b| &mut b.instructions) {
            if let Op::Phi = inst.class.opcode {
                if let Some(&Operand::IdRef(base)) = inst.operands.first() {
                    if let Some(&(_, result_type)) = ops.get_by_left(&base) {
                        inst.result_type.replace(result_type);
                        for var in inst.operands.iter().step_by(2) {
                            let result_id = var.unwrap_id_ref();
                            if !ops.contains_left(&result_id) {
                                changes.insert(result_id, result_type);
                            }
                        }
                    }
                }
            }
        }
    }

    for inst in &mut module.types_global_values {
        if let Some(result_id) = inst.result_id {
            if let Some(&result_type) = changes.get(&result_id) {
                inst.result_type.replace(result_type);
            }
        }
    }

    if !changes.is_empty() {
        // insert pointers after pointee declaration
        let types_global_values = module
            .types_global_values
            .iter()
            .cloned()
            .flat_map(|mut inst| {
                if let Some(result_id) = inst.result_id {
                    if let Some(&result_type) = changes.get(&result_id) {
                        inst.result_type.replace(result_type);
                    }
                    if let Some(pointers) = new_pointers.get_mut(&result_id) {
                        let pointers = core::mem::replace(pointers, Vec::new());
                        once(inst).chain(pointers)
                    } else {
                        once(inst).chain(Vec::new())
                    }
                } else {
                    once(inst).chain(Vec::new())
                }
            })
            .collect();
        module.types_global_values = types_global_values;
    }
}
