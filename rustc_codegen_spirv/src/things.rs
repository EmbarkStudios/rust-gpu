use crate::ctx::type_tracker::SpirvType;
use rspirv::spirv::Word;
use rustc_codegen_ssa::traits::{ModuleBufferMethods, ThinBufferMethods};
use rustc_middle::ty::TyCtxt;

// uncategorized things, should be put into modules eventually

pub struct SpirvModuleBuffer;

impl ModuleBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        todo!()
    }
}

pub struct SprivThinBuffer;

impl ThinBufferMethods for SprivThinBuffer {
    fn data(&self) -> &[u8] {
        todo!()
    }
}

pub struct ModuleSpirv {}

impl ModuleSpirv {
    pub fn new<'tcx>(tcx: TyCtxt<'tcx>, cgu_name: &str) -> Self {
        Self {}
    }
}
