mod local_tracker;

use local_tracker::LocalTracker;
use rspirv::binary::Assemble;
use rspirv::dr::Builder;
use rspirv::spirv::{AddressingModel, Capability, MemoryModel, Word};
use rustc_middle::mir::BasicBlock;
use rustc_middle::ty::TyCtxt;
use std::collections::HashMap;
use std::hash::Hash;

pub struct Context<'tcx> {
    pub spirv: Builder,
    pub tcx: TyCtxt<'tcx>,
    pub current_function_is_void: bool,
    basic_blocks: ForwardReference<BasicBlock>,
    pub locals: LocalTracker,
}

impl<'tcx> Context<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        let mut spirv = Builder::new();
        spirv.capability(Capability::Shader);
        // Temp hack: Linkage allows us to get away with no OpEntryPoint
        spirv.capability(Capability::Linkage);
        spirv.memory_model(AddressingModel::Logical, MemoryModel::GLSL450);
        Self {
            spirv,
            tcx,
            current_function_is_void: false,
            basic_blocks: ForwardReference::new(),
            locals: LocalTracker::new(),
        }
    }

    pub fn assemble(self) -> Vec<u32> {
        self.spirv.module().assemble()
    }

    pub fn get_basic_block(&mut self, bb: BasicBlock) -> Word {
        self.basic_blocks.get(&mut self.spirv, bb)
    }

    pub fn clear_after_fn(&mut self) {
        self.basic_blocks.clear();
        self.locals.clear();
    }
}

struct ForwardReference<T: Eq + Hash> {
    values: HashMap<T, Word>,
}

impl<T: Eq + Hash> ForwardReference<T> {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn get(&mut self, builder: &mut Builder, key: T) -> Word {
        *self.values.entry(key).or_insert_with(|| builder.id())
    }

    fn clear(&mut self) {
        self.values.clear();
    }
}
