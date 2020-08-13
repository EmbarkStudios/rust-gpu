use rspirv::spirv::Word;
use rustc_middle::mir::Local;
use std::collections::HashMap;

// TODO: Expand this to SSA-conversion
pub struct LocalTracker {
    locals: HashMap<Local, SpirvLocal>,
}

impl LocalTracker {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }

    pub fn def(&mut self, local: Local, spirv_local: SpirvLocal) {
        self.locals.insert(local, spirv_local);
    }

    pub fn get(&self, local: &Local) -> SpirvLocal {
        self.locals[local]
    }
}

/*
pub enum SpirvLocalKind {
    ReturnValue,
    Argument,
    Local,
}
*/

#[derive(Copy, Clone)]
pub struct SpirvLocal {
    pub local_type: Word,
    pub local_ptr_type: Word,
    pub op_variable_pointer: Word,
}

impl SpirvLocal {
    pub fn new(local_type: Word, local_ptr_type: Word, op_variable_pointer: Word) -> Self {
        Self {
            local_type,
            local_ptr_type,
            op_variable_pointer,
        }
    }
}
