use rspirv::dr::{Instruction, Module, Operand};
use std::collections::HashMap;

/// `DefAnalyzer` is a simple lookup table for instructions: Sometimes, we have a spirv
/// `result_id`, and we want to find the corresponding instruction. This struct loops over all
/// instructions in the module (expensive!) and builds a table from `result_id` to its defining
/// instruction. Note that it holds a reference to the instruction, instead of cloning it. While we
/// really could clone it, it's nice to keep the reference here, since then rustc guarantees we do
/// not mutate the module while a `DefAnalyzer` is alive (which would be really bad).
pub struct DefAnalyzer<'a> {
    def_ids: HashMap<u32, &'a Instruction>,
}

impl<'a> DefAnalyzer<'a> {
    pub fn new(module: &'a Module) -> Self {
        let mut def_ids = HashMap::new();

        module.all_inst_iter().for_each(|inst| {
            if let Some(def_id) = inst.result_id {
                def_ids
                    .entry(def_id)
                    .and_modify(|stored_inst| {
                        *stored_inst = inst;
                    })
                    .or_insert_with(|| inst);
            }
        });

        Self { def_ids }
    }

    pub fn def(&self, id: u32) -> Option<&'a Instruction> {
        self.def_ids.get(&id).copied()
    }

    /// Helper that extracts the operand as an `IdRef` and then looks up that id's instruction.
    ///
    /// # Panics
    ///
    /// Panics when provided an operand that doesn't reference an id, or that id is missing.
    pub fn op_def(&self, operand: &Operand) -> Instruction {
        self.def(operand.id_ref_any().expect("Expected ID"))
            .unwrap()
            .clone()
    }
}
