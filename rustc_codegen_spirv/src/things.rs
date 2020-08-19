use rustc_codegen_ssa::traits::{ModuleBufferMethods, ThinBufferMethods};

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
