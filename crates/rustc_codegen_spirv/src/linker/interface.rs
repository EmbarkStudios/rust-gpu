//! Entrypoint interface legalization.
//!
//! SPIR-V entrypoint definitions require a listing of the used interface variables.
//! Features like `bindless` define global interface variables which may not be used and specialized.
//!
//! The entrypoint definition in the builder will not specify them ad-hoc but we infer in this
//! legalization pass for every entrypoint the required variables and add them to the definitions.

use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Op, StorageClass};
use rustc_data_structures::fx::{FxHashSet, FxHashMap};

pub fn legalize_entrypoint(module: &mut Module) {
    let version = module.header.as_ref().unwrap().version();

    // Map: Id -> Function Index
    let mut functions = FxHashMap::default();
    for (i, inst) in module.functions.iter().enumerate() {
        let id = inst.def.as_ref().unwrap().result_id.unwrap();
        functions.insert(id, i);
    }

    // Generate list of all global variables which could be
    // used by an entrypoint, filtered according to SPIR-V version requirements.
    let mut global_variables = FxHashSet::default();
    for inst in &module.types_global_values {
        if inst.class.opcode != Op::Variable {
            continue;
        }

        let storage_class = if let Operand::StorageClass(storage_class) = inst.operands[0] {
            storage_class
        } else {
            continue
        };

        let is_interface = if version >= (1, 4) {
            // SPIR-V >= v1.4 includes all OpVariables in the interface.
            storage_class != StorageClass::Function
        } else {
            // SPIR-V <= v1.3 only includes Input and Output in the interface.
            storage_class == StorageClass::Input || storage_class == StorageClass::Output
        };

        if is_interface {
            global_variables.insert(inst.result_id.unwrap());
        }
    }

    // Walk the call graph of each entrypoint.
    // For each instruction we will the operands if they use any global variable.
    for entry in &mut module.entry_points {
        let entry_point = entry.operands[1].id_ref_any().unwrap();

        let mut interfaces = FxHashSet::default();
        let mut visited = FxHashSet::default();
        let mut frontier = vec![entry_point];
        while let Some(fn_id) = frontier.pop() {
            visited.insert(fn_id);

            let fn_idx = functions[&fn_id];
            let function = &module.functions[fn_idx];
            for inst in function.all_inst_iter() {
                if inst.class.opcode == Op::FunctionCall {
                    let called_fn = inst.operands[0].id_ref_any().unwrap();
                    if !visited.contains(&called_fn) {
                        frontier.push(called_fn);
                    }
                }

                // Check operands for global variable usage.
                for op in &inst.operands {
                    if let Operand::IdRef(id) = op {
                        if global_variables.contains(id) {
                            interfaces.insert(id);
                        }
                    }
                }
            }
        }

        // Patch up the entrypoint definition by appending/replacing the operand list.
        let mut operands = entry.operands[..3].to_vec();
        operands.extend(interfaces.into_iter().map(|id| Operand::IdRef(*id)));
        entry.operands = operands;
    }
}
