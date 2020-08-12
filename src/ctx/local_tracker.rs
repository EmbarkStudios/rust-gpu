use rspirv::spirv::Word;
use rustc_middle::mir::Local;
use std::collections::HashMap;

// TODO: Expand this to SSA-conversion
pub struct LocalTracker {
    locals: HashMap<Local, Word>,
}

impl LocalTracker {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }

    pub fn def(&mut self, local: Local, expr: Word) {
        match self.locals.entry(local) {
            std::collections::hash_map::Entry::Occupied(_) => {
                println!("Non-SSA code not supported yet")
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(expr);
            }
        }
    }

    pub fn get(&self, local: Local) -> Word {
        // This probably needs to be fixed, forward-references might be a thing
        *self.locals.get(&local).expect("Undefined local")
    }
}
