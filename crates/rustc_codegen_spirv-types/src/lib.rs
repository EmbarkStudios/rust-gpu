//! Types used by both `rustc_codegen_spirv` and `spirv-builder`.

pub use rspirv::spirv::Capability;

mod compile_result;
pub use compile_result::*;
