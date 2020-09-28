use crate::DefAnalyzer;
use crate::{operand_idref, operand_idref_mut};
use rspirv::spirv;
use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::mem::replace;
use topological_sort::TopologicalSort;

pub fn shift_ids(module: &mut rspirv::dr::Module, add: u32) {
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

pub fn block_ordering_pass(func: &mut rspirv::dr::Function) {
    if func.blocks.len() < 2 {
        return;
    }
    fn visit_postorder(
        func: &rspirv::dr::Function,
        visited: &mut HashSet<spirv::Word>,
        postorder: &mut Vec<spirv::Word>,
        current: spirv::Word,
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

fn label_of(block: &rspirv::dr::Block) -> spirv::Word {
    block.label.as_ref().unwrap().result_id.unwrap()
}

fn outgoing_edges(block: &rspirv::dr::Block) -> Vec<spirv::Word> {
    fn unwrap_id_ref(operand: &rspirv::dr::Operand) -> spirv::Word {
        match *operand {
            rspirv::dr::Operand::IdRef(word) => word,
            _ => panic!("Expected Operand::IdRef: {}", operand),
        }
    }
    let terminator = block.instructions.last().unwrap();
    // https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#Termination
    match terminator.class.opcode {
        spirv::Op::Branch => vec![unwrap_id_ref(&terminator.operands[0])],
        spirv::Op::BranchConditional => vec![
            unwrap_id_ref(&terminator.operands[1]),
            unwrap_id_ref(&terminator.operands[2]),
        ],
        spirv::Op::Switch => once(unwrap_id_ref(&terminator.operands[1]))
            .chain(
                terminator.operands[3..]
                    .iter()
                    .step_by(2)
                    .map(unwrap_id_ref),
            )
            .collect(),
        spirv::Op::Return | spirv::Op::ReturnValue | spirv::Op::Kill | spirv::Op::Unreachable => {
            Vec::new()
        }
        _ => panic!("Invalid block terminator: {:?}", terminator),
    }
}

pub fn compact_ids(module: &mut rspirv::dr::Module) -> u32 {
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

pub fn max_bound(module: &rspirv::dr::Module) -> u32 {
    let mut max = 0;
    for inst in module.all_inst_iter() {
        if let Some(result_id) = inst.result_id {
            max = max.max(result_id);
        }
        if let Some(result_type) = inst.result_type {
            max = max.max(result_type);
        }
        inst.operands.iter().for_each(|op| {
            if let Some(w) = operand_idref(op) {
                max = max.max(w);
            }
        })
    }
    max + 1
}

pub fn sort_globals(module: &mut rspirv::dr::Module) {
    let mut ts = TopologicalSort::<u32>::new();

    for t in module.types_global_values.iter() {
        if let Some(result_id) = t.result_id {
            if let Some(result_type) = t.result_type {
                ts.add_dependency(result_type, result_id);
            }

            for op in &t.operands {
                if let Some(w) = operand_idref(op) {
                    ts.add_dependency(w, result_id); // the op defining the IdRef should come before our op / result_id
                }
            }
        }
    }

    let defs = DefAnalyzer::new(&module);

    let mut new_types_global_values = vec![];

    loop {
        if ts.is_empty() {
            break;
        }

        let mut v = ts.pop_all();
        v.sort_unstable();

        for result_id in v {
            new_types_global_values.push(defs.def(result_id).unwrap().clone());
        }
    }

    assert!(module.types_global_values.len() == new_types_global_values.len());

    module.types_global_values = new_types_global_values;

    // defs go before fns
    module.functions.sort_by_key(|f| !f.blocks.is_empty());
}
