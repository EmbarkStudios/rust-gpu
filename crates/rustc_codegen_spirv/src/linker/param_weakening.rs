//! Interprocedural optimizations that "weaken" function parameters, i.e. they
//! replace parameter types with "simpler" ones, or outright remove parameters,
//! based on how those parameters are used in the function and/or what arguments
//! get passed from callers.
//!
use crate::linker::ipo::CallGraph;
use indexmap::IndexMap;
use rspirv::dr::{Builder, Module, Operand};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::FxHashMap;
use rustc_index::bit_set::BitSet;
use std::mem;

pub fn remove_unused_params(module: Module) -> Module {
    let call_graph = CallGraph::collect(&module);

    // Gather all of the unused parameters for each function, transitively.
    // (i.e. parameters which are passed, as call arguments, to functions that
    // won't use them, are also considered unused, through any number of calls)
    let mut unused_params_per_func_id: IndexMap<Word, BitSet<usize>> = IndexMap::new();
    for func_idx in call_graph.post_order() {
        // Skip entry points, as they're the only "exported" functions, at least
        // at link-time (likely only relevant to `Kernel`s, but not `Shader`s).
        if call_graph.entry_points.contains(&func_idx) {
            continue;
        }

        let func = &module.functions[func_idx];

        let params_id_to_idx: FxHashMap<Word, usize> = func
            .parameters
            .iter()
            .enumerate()
            .map(|(i, p)| (p.result_id.unwrap(), i))
            .collect();
        let mut unused_params = BitSet::new_filled(func.parameters.len());
        for inst in func.all_inst_iter() {
            // If this is a call, we can ignore the arguments passed to the
            // callee parameters we already determined to be unused, because
            // those parameters (and matching arguments) will get removed later.
            let (operands, ignore_operands) = if inst.class.opcode == Op::FunctionCall {
                (
                    &inst.operands[1..],
                    unused_params_per_func_id.get(&inst.operands[0].unwrap_id_ref()),
                )
            } else {
                (&inst.operands[..], None)
            };

            for (i, operand) in operands.iter().enumerate() {
                if let Some(ignore_operands) = ignore_operands {
                    if ignore_operands.contains(i) {
                        continue;
                    }
                }

                if let Operand::IdRef(id) = operand {
                    if let Some(&param_idx) = params_id_to_idx.get(id) {
                        unused_params.remove(param_idx);
                    }
                }
            }
        }

        if !unused_params.is_empty() {
            unused_params_per_func_id.insert(func.def_id().unwrap(), unused_params);
        }
    }

    // Remove unused parameters and call arguments for unused parameters.
    let mut builder = Builder::new_from_module(module);
    for func_idx in 0..builder.module_ref().functions.len() {
        let func = &mut builder.module_mut().functions[func_idx];
        let unused_params = unused_params_per_func_id.get(&func.def_id().unwrap());
        if let Some(unused_params) = unused_params {
            func.parameters = mem::take(&mut func.parameters)
                .into_iter()
                .enumerate()
                .filter(|&(i, _)| !unused_params.contains(i))
                .map(|(_, p)| p)
                .collect();
        }

        for inst in func.all_inst_iter_mut() {
            if inst.class.opcode == Op::FunctionCall {
                if let Some(unused_callee_params) =
                    unused_params_per_func_id.get(&inst.operands[0].unwrap_id_ref())
                {
                    inst.operands = mem::take(&mut inst.operands)
                        .into_iter()
                        .enumerate()
                        .filter(|&(i, _)| i == 0 || !unused_callee_params.contains(i - 1))
                        .map(|(_, o)| o)
                        .collect();
                }
            }
        }

        // Regenerate the function type from remaining parameters, if necessary.
        if unused_params.is_some() {
            let return_type = func.def.as_mut().unwrap().result_type.unwrap();
            let new_param_types: Vec<_> = func
                .parameters
                .iter()
                .map(|inst| inst.result_type.unwrap())
                .collect();
            let new_func_type = builder.type_function(return_type, new_param_types);
            let func = &mut builder.module_mut().functions[func_idx];
            func.def.as_mut().unwrap().operands[1] = Operand::IdRef(new_func_type);
        }
    }

    builder.module()
}
