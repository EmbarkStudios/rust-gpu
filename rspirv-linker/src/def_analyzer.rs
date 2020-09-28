use crate::operand_idref;
use std::collections::HashMap;

pub struct DefAnalyzer {
    def_ids: HashMap<u32, rspirv::dr::Instruction>,
}

impl DefAnalyzer {
    pub fn new(module: &rspirv::dr::Module) -> Self {
        let mut def_ids = HashMap::new();

        module.all_inst_iter().for_each(|inst| {
            if let Some(def_id) = inst.result_id {
                def_ids
                    .entry(def_id)
                    .and_modify(|stored_inst| {
                        *stored_inst = inst.clone();
                    })
                    .or_insert_with(|| inst.clone());
            }
        });

        Self { def_ids }
    }

    pub fn def(&self, id: u32) -> Option<&rspirv::dr::Instruction> {
        self.def_ids.get(&id)
    }

    pub fn op_def(&self, operand: &rspirv::dr::Operand) -> rspirv::dr::Instruction {
        self.def(operand_idref(operand).expect("Expected ID"))
            .unwrap()
            .clone()
    }
}
