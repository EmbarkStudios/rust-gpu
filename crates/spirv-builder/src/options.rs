//! Various options for building SPIR-V modules and relevant components.

mod source;

use std::fmt;

pub use source::{Source, SourceKind};

/// Options for compiling `librustc_codegen_spirv`.
#[derive(Default)]
pub struct CodegenBuildOptions {
    /// Whether to compile in release mode. Default: `false`
    pub release: bool,
    /// Whether to use installed SPIR-V tools, or compile them. Default: `Auto`
    pub spirv_tools: SpirvTools,
}

/// Sets the memory model to use for the SPIR-V module. See [Memory Model]
/// from the SPIR-V specification for more information.
///
/// [memory model]: https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#_a_id_memorymodelsection_a_memory_model
pub enum MemoryModel {
    /// No shared memory consistency issues. Capabilities: Shader
    Simple,
    /// Vulkan memory model, as specified by the client API. This memory model
    /// must be declared if and only if the `VulkanMemoryModel` capability
    /// is declared. Capabilities: VulkanMemoryModel
    Vulkan,
    /// Memory model needed by later versions of GLSL and ESSL. Works across
    /// multiple versions. Capabilities: Shader
    Glsl450,
}

impl fmt::Display for MemoryModel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MemoryModel::Simple => "simple",
                MemoryModel::Vulkan => "vulkan",
                MemoryModel::Glsl450 => "glsl450",
            }
        )
    }
}

/// How to compile SPIR-V tools.
pub enum SpirvTools {
    /// Detect whether SPIR-V tools are present in the environment and use
    /// those, else compile them.
    Auto,
    /// Compile SPIR-V tools from source.
    Compiled,
    /// Use SPIR-V tools available in the environment.
    Installed,
}

impl Default for SpirvTools {
    fn default() -> Self {
        Self::Auto
    }
}

impl fmt::Display for SpirvTools {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const COMPILED_FEATURE: &str = "use-compiled-tools";
        const INSTALLED_FEATURE: &str = "use-installed-tools";
        write!(
            f,
            "{}",
            match self {
                Self::Auto => {
                    if std::process::Command::new("spirv-val")
                        .args(&["--version"])
                        .status()
                        .map_or(false, |s| s.success())
                    {
                        INSTALLED_FEATURE
                    } else {
                        COMPILED_FEATURE
                    }
                }
                Self::Compiled => COMPILED_FEATURE,
                Self::Installed => INSTALLED_FEATURE,
            }
        )
    }
}

/// The version of SPIR-V to when compiling.
#[derive(Clone, Copy)]
pub(crate) struct Version {
    pub major: u8,
    pub minor: u8,
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "spirv{}.{}", self.major, self.minor)
    }
}
