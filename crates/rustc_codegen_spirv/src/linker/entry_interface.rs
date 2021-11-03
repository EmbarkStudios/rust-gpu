//! Passes that pertain to `OpEntryPoint`'s "interface variables".

use crate::linker::ipo::CallGraph;
use indexmap::{IndexMap, IndexSet};
use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Op, StorageClass, Word};
use std::mem;

type Id = Word;

/// Update `OpEntryPoint`s to contain all of the `OpVariable`s they reference,
/// whether directly or through some function in their call graph.
///
/// This is needed for (arguably-not-interface) `Private` in SPIR-V >= 1.4,
/// but also any interface variables declared "out of band" (e.g. via `asm!`).
pub fn gather_all_interface_vars_from_uses(module: &mut Module) {
    // Start by mapping out which global (i.e. `OpVariable` or constants) IDs
    // can be used to access any interface-relevant `OpVariable`s
    // (where "interface-relevant" depends on the version, see comments below).
    let mut used_vars_per_global_id: IndexMap<Id, IndexSet<Id>> = IndexMap::new();
    let version = module.header.as_ref().unwrap().version();
    for inst in &module.types_global_values {
        let mut used_vars = IndexSet::new();

        // Base case: the global itself is an interface-relevant `OpVariable`.
        let interface_relevant_var = inst.class.opcode == Op::Variable && {
            if version > (1, 3) {
                // SPIR-V >= v1.4 includes all OpVariables in the interface.
                true
            } else {
                let storage_class = inst.operands[0].unwrap_storage_class();
                // SPIR-V <= v1.3 only includes Input and Output in the interface.
                storage_class == StorageClass::Input || storage_class == StorageClass::Output
            }
        };
        if interface_relevant_var {
            used_vars.insert(inst.result_id.unwrap());
        }

        // Nested constant refs (e.g. `&&&0`) can create chains of `OpVariable`s
        // where only the outer-most `OpVariable` may be accessed directly,
        // but the interface variables need to include all the nesting levels.
        used_vars.extend(
            inst.operands
                .iter()
                .filter_map(|operand| operand.id_ref_any())
                .filter_map(|id| used_vars_per_global_id.get(&id))
                .flatten(),
        );

        if !used_vars.is_empty() {
            used_vars_per_global_id.insert(inst.result_id.unwrap(), used_vars);
        }
    }

    // Initial uses come from functions directly referencing global instructions.
    let mut used_vars_per_fn_idx: Vec<IndexSet<Id>> = module
        .functions
        .iter()
        .map(|func| {
            func.all_inst_iter()
                .flat_map(|inst| &inst.operands)
                .filter_map(|operand| operand.id_ref_any())
                .filter_map(|id| used_vars_per_global_id.get(&id))
                .flatten()
                .copied()
                .collect()
        })
        .collect();

    // Uses can then be propagated through the call graph, from callee to caller.
    let call_graph = CallGraph::collect(module);
    for caller_idx in call_graph.post_order() {
        let mut used_vars = mem::take(&mut used_vars_per_fn_idx[caller_idx]);
        for &callee_idx in &call_graph.callees[caller_idx] {
            used_vars.extend(&used_vars_per_fn_idx[callee_idx]);
        }
        used_vars_per_fn_idx[caller_idx] = used_vars;
    }

    // All transitive uses are available, add them to `OpEntryPoint`s.
    for (i, entry) in module.entry_points.iter_mut().enumerate() {
        assert_eq!(entry.class.opcode, Op::EntryPoint);
        let &entry_func_idx = call_graph.entry_points.get_index(i).unwrap();
        assert_eq!(
            module.functions[entry_func_idx].def_id().unwrap(),
            entry.operands[1].unwrap_id_ref()
        );

        // NOTE(eddyb) it might be better to remove any unused vars, or warn
        // the user about their presence, but for now this keeps them around.
        let mut interface_vars: IndexSet<Id> = entry.operands[3..]
            .iter()
            .map(|operand| operand.unwrap_id_ref())
            .collect();

        interface_vars.extend(&used_vars_per_fn_idx[entry_func_idx]);

        entry.operands.truncate(3);
        entry
            .operands
            .extend(interface_vars.iter().map(|&id| Operand::IdRef(id)));
    }
}
