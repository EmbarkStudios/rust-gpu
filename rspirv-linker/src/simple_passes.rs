use crate::DefAnalyzer;
use crate::{operand_idref, operand_idref_mut};
use std::collections::HashMap;
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
}
