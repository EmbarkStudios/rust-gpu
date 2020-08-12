mod local_tracker;

use local_tracker::LocalTracker;
use rspirv::binary::Assemble;
use rspirv::dr::Builder;
use rspirv::spirv::{AddressingModel, Capability, MemoryModel, StorageClass, Word};
use rustc_middle::mir::BasicBlock;
use rustc_middle::ty::{subst::SubstsRef, TyCtxt};
use std::collections::HashMap;
use std::hash::Hash;

pub struct Context<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub spirv: Builder,
    spirv_helper: SpirvHelper,
}

impl<'tcx> Context<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        let mut spirv = Builder::new();
        spirv.capability(Capability::Shader);
        // Temp hack: Linkage allows us to get away with no OpEntryPoint
        spirv.capability(Capability::Linkage);
        spirv.memory_model(AddressingModel::Logical, MemoryModel::GLSL450);
        Self {
            tcx,
            spirv,
            spirv_helper: SpirvHelper::new(),
        }
    }

    pub fn assemble(self) -> Vec<u32> {
        self.spirv.module().assemble()
    }
}

pub struct FnCtx<'ctx, 'tcx> {
    pub ctx: &'ctx mut Context<'tcx>,
    pub substs: SubstsRef<'tcx>,
    pub is_void: bool,
    basic_blocks: ForwardReference<BasicBlock>,
    pub locals: LocalTracker,
}

impl<'ctx, 'tcx> FnCtx<'ctx, 'tcx> {
    pub fn new(ctx: &'ctx mut Context<'tcx>, substs: SubstsRef<'tcx>) -> Self {
        Self {
            ctx,
            substs,
            is_void: false,
            basic_blocks: ForwardReference::new(),
            locals: LocalTracker::new(),
        }
    }

    pub fn tcx(&mut self) -> &mut TyCtxt<'tcx> {
        &mut self.ctx.tcx
    }

    pub fn spirv(&mut self) -> &mut Builder {
        &mut self.ctx.spirv
    }

    pub fn get_basic_block(&mut self, bb: BasicBlock) -> Word {
        self.basic_blocks.get(&mut self.ctx.spirv, bb)
    }

    pub fn type_pointer(&mut self, pointee_type: Word) -> Word {
        self.ctx
            .spirv_helper
            .type_pointer(&mut self.ctx.spirv, pointee_type)
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
}

struct SpirvHelper {
    pointer: HashMap<Word, Word>,
}

impl SpirvHelper {
    fn new() -> Self {
        Self {
            pointer: HashMap::new(),
        }
    }

    fn type_pointer(&mut self, spirv: &mut Builder, pointee_type: Word) -> Word {
        // TODO: StorageClass
        *self
            .pointer
            .entry(pointee_type)
            .or_insert_with(|| spirv.type_pointer(None, StorageClass::Generic, pointee_type))
    }
}
