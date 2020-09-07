use rustc_codegen_ssa::traits::{ModuleBufferMethods, ThinBufferMethods};

// uncategorized things, should be put into modules eventually

pub struct SpirvModuleBuffer(pub Vec<u32>);

impl ModuleBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        crate::slice_u32_to_u8(&self.0)
    }
}

pub struct SpirvThinBuffer(pub Vec<u32>);

impl ThinBufferMethods for SpirvThinBuffer {
    fn data(&self) -> &[u8] {
        crate::slice_u32_to_u8(&self.0)
    }
}
