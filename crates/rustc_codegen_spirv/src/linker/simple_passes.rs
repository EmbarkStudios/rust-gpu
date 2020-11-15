use rspirv::dr::{Block, Function, Module, Instruction, Operand};
use rspirv::spirv::{Op, Word, StorageClass};
use std::collections::{HashMap, HashSet};
use bimap::BiHashMap;
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
// the old pointer types may then be unused 
pub fn match_variable_storage_classes(module: &mut Module) {
    let mut variable_to_storage_class = HashMap::<Word, StorageClass>::new();
    let mut pointer_to_storage_class_pointee = BiHashMap::<Word, (StorageClass, Word)>::new();
    let mut id = module.global_inst_iter().count() as Word;
    
    fn get_variable(instr: &Instruction) -> Option<(Word, StorageClass)> {
        let result_id = instr.result_id?;
        if let &[Operand::StorageClass(storage_class)] = &*instr.operands {
            Some((result_id, storage_class))
        } else {
            None
        }
    } 
    
    fn get_pointer(instr: &Instruction) -> Option<(Word, (StorageClass, Word))> {
        let pointer = instr.result_id?;
        if let &[Operand::StorageClass(storage_class), Operand::IdRef(pointee)] = &*instr.operands {
            Some((pointer, (storage_class, pointee)))
        } else {
            None
        }
    } 
    
    // Get variables and pointers
    for instr in &module.types_global_values {
        match instr.class.opcode {
            Op::Variable => {
                if let Some((result_id, storage_class)) = get_variable(instr) {
                    let prev = variable_to_storage_class.insert(result_id, storage_class);
                    assert!(prev.is_none());
                }
            },
            Op::TypePointer => {
                if let Some((pointer, (storage_class, pointee))) = get_pointer(instr) {
                    pointer_to_storage_class_pointee.insert_no_overwrite(pointer, (storage_class, pointee)).unwrap();
                }
            },
            _ => (),
        }
    }
    
    let mut types_global_values = &mut module.types_global_values;
    
    let mut update_op = |instr: &mut Instruction| {
        let op_pointer = instr.result_type?;
        if let &[Operand::IdRef(base_id), ..] = &*instr.operands {
            let base_storage_class = *variable_to_storage_class.get(&base_id)?;
            let &(storage_class, pointee) = pointer_to_storage_class_pointee.get_by_left(&op_pointer)?;
            if storage_class != base_storage_class {
                // Create a new pointer with base_storage_class
                // If it exists, use that id
                if let Some(&pointer) = pointer_to_storage_class_pointee.get_by_right(&(base_storage_class, pointee)) {
                    instr.result_type.replace(pointer);
                } else { // otherwise create it
                    instr.result_type.replace(id);
                    pointer_to_storage_class_pointee.insert_no_overwrite(id, (base_storage_class, pointee)).unwrap();
                    types_global_values.push(Instruction::new(
                        Op::TypePointer,
                        None,
                        Some(id),
                        vec![
                            Operand::StorageClass(base_storage_class),
                            Operand::IdRef(pointee)
                        ]
                    ));
                    id += 1;           
                }
            }    
        }
        Some(())
    };          
    
    let func_instr_iter = module
        .functions
        .iter_mut()
        .flat_map(|f| &mut f.blocks)
        .flat_map(|b| &mut b.instructions);
    
    // Update access chains with new pointer result type
    for instr in func_instr_iter {
        match instr.class.opcode {
            Op::AccessChain | Op::InBoundsAccessChain => {
                update_op(instr);
            },
            _ => (),
        }
    }
    
    
}
