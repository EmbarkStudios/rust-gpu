#[cfg(feature = "use-compiled")]
pub mod compiled;

#[cfg(feature = "use-installed")]
pub mod tool;

#[derive(Copy, Clone, Default)]
pub struct AssemblerOptions {
    /// Numeric IDs in the binary will have the same values as in the source.
    /// Non-numeric IDs are allocated by filling in the gaps, starting with 1
    /// and going up.
    pub preserve_numeric_ids: bool,
}

impl Into<u32> for AssemblerOptions {
    fn into(self) -> u32 {
        // This is weird, the "none" is 1, so I'm not sure if that means having
        // it disables all other options or...?
        let mut res = 0; //assembler::BinaryOptions::None as u32;

        if self.preserve_numeric_ids {
            res |= spirv_tools_sys::assembler::BinaryOptions::PreserveNumberIds as u32;
        }

        res
    }
}

pub trait Assembler: Default {
    fn with_env(target_env: crate::TargetEnv) -> Self;
    fn assemble(
        &self,
        text: &str,
        options: AssemblerOptions,
    ) -> Result<crate::binary::Binary, crate::error::Error>;
}