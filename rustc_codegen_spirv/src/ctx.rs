mod local_tracker;

use local_tracker::LocalTracker;
use rspirv::binary::Assemble;
use rspirv::dr::Builder;
use rspirv::spirv::{AddressingModel, Capability, MemoryModel, StorageClass, Word};
use rustc_middle::mir::{BasicBlock, Body};
use rustc_middle::ty::subst::SubstsRef;
use rustc_middle::ty::{fold::TypeFoldable, Instance, ParamEnv, TyCtxt};
use rustc_span::def_id::DefId;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Copy, Clone)]
pub enum PointerSize {
    P32,
    P64,
}

impl PointerSize {
    pub fn val(self) -> u64 {
        match self {
            PointerSize::P32 => 32,
            PointerSize::P64 => 64,
        }
    }
}

/// Context is the "bag of random variables" used for global state - i.e. variables that live the whole compilation.
pub struct Context<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub spirv: Builder,
    spirv_helper: SpirvHelper,
    pub pointer_size: PointerSize,
    func_defs: ForwardReference<(DefId, SubstsRef<'tcx>)>,
}

impl<'tcx> Context<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        let mut spirv = Builder::new();
        spirv.capability(Capability::Shader);
        // Temp hack: Linkage allows us to get away with no OpEntryPoint
        spirv.capability(Capability::Linkage);
        spirv.memory_model(AddressingModel::Logical, MemoryModel::GLSL450);
        let _ = PointerSize::P32; // just mark as used
        Self {
            tcx,
            spirv,
            spirv_helper: SpirvHelper::new(),
            pointer_size: PointerSize::P64,
            func_defs: ForwardReference::new(),
        }
    }

    pub fn assemble(self) -> Vec<u32> {
        self.spirv.module().assemble()
    }

    pub fn get_func_def(&mut self, id: DefId, substs: SubstsRef<'tcx>) -> Word {
        self.func_defs.get(&mut self.spirv, (id, substs))
    }
}

/// FnCtx is the "bag of random variables" used for state when compiling a particular function - i.e. variables that are
/// specific to compiling a particular function. Note it carries a reference to the global Context, so those variables
/// can be accessed too.
pub struct FnCtx<'ctx, 'tcx> {
    /// The global state - note this field is `pub`, so if needed, you can go through this field, instead of the helper
    /// functions, to satisfy the borrow checker (since using the helper functions borrow this whole struct).
    pub ctx: &'ctx mut Context<'tcx>,
    pub instance: Instance<'tcx>,
    pub body: &'tcx Body<'tcx>,
    pub is_void: bool,
    basic_blocks: ForwardReference<BasicBlock>,
    pub locals: LocalTracker,
}

impl<'ctx, 'tcx> FnCtx<'ctx, 'tcx> {
    pub fn new(
        ctx: &'ctx mut Context<'tcx>,
        instance: Instance<'tcx>,
        body: &'tcx Body<'tcx>,
    ) -> Self {
        Self {
            ctx,
            instance,
            body,
            is_void: false,
            basic_blocks: ForwardReference::new(),
            locals: LocalTracker::new(),
        }
    }

    /*
    pub fn tcx(&mut self) -> &mut TyCtxt<'tcx> {
        &mut self.ctx.tcx
    }
    */

    pub fn spirv(&mut self) -> &mut Builder {
        &mut self.ctx.spirv
    }

    /// Gets the spir-v label for a basic block, or generates one if it doesn't exist.
    pub fn get_basic_block(&mut self, bb: BasicBlock) -> Word {
        self.basic_blocks.get(&mut self.ctx.spirv, bb)
    }

    /// rspirv doesn't cache type_pointer, so cache it ourselves here.
    pub fn type_pointer(&mut self, pointee_type: Word) -> Word {
        self.ctx
            .spirv_helper
            .type_pointer(&mut self.ctx.spirv, pointee_type)
    }

    // copied from rustc_codegen_cranelift
    pub(crate) fn monomorphize<T>(&self, value: &T) -> T
    where
        T: TypeFoldable<'tcx> + Copy,
    {
        if let Some(substs) = self.instance.substs_for_mir_body() {
            self.ctx
                .tcx
                .subst_and_normalize_erasing_regions(substs, ParamEnv::reveal_all(), value)
        } else {
            self.ctx
                .tcx
                .normalize_erasing_regions(ParamEnv::reveal_all(), *value)
        }
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
