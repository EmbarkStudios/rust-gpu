use super::type_tracker::SpirvType;
use rspirv::spirv::Word;
use rustc_middle::mir::Local;
use std::collections::HashMap;

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

    pub fn get(&self, local: &Local) -> &SpirvLocal {
        &self.locals[local]
    }
}

/*
pub enum SpirvLocalKind {
    ReturnValue,
    Argument,
    Local,
}
*/

pub struct SpirvLocal {
    pub ty: SpirvType,
    pub def: Word,
}

impl SpirvLocal {
    pub fn new(ty: SpirvType, def: Word) -> Self {
        if let SpirvType::Pointer { .. } = ty {
        } else {
            panic!("Local type not a pointer: {:?}", ty);
        }
        Self { ty, def }
    }

    pub fn pointee_ty(&self) -> &SpirvType {
        if let SpirvType::Pointer { pointee, .. } = &self.ty {
            pointee
        } else {
            panic!("Local type not a pointer: {:?}", self.ty);
        }
    }
}
