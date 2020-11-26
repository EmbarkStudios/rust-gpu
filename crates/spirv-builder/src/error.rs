use std::fmt;

/// An error occurred while Building the SPIR-V module.
#[derive(Debug)]
pub struct SpirvBuilderError;

impl fmt::Display for SpirvBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Build failed")
    }
}

impl std::error::Error for SpirvBuilderError {}
