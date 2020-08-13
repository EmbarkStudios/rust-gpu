use rspirv::spirv::Word;
use rustc_span::def_id::DefId;
use std::collections::HashMap;

pub struct TypeTracker {
    pub adts: HashMap<DefId, SpirvAdt>,
}

impl TypeTracker {
    pub fn new() -> Self {
        Self {
            adts: HashMap::new(),
        }
    }
}

// This uses the rustc definition of "adt", i.e. a struct, enum, or union
pub struct SpirvAdt {
    pub spirv_id: Word,
    // TODO: enums/unions
    pub field_types: Vec<Word>,
}
