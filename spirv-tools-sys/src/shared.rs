use std::fmt;

/// Certain target environments impose additional restrictions on SPIR-V, so it's
/// often necessary to specify which one applies. `Universal_*` implies an
/// environment-agnostic SPIR-V.
///
/// When an API method needs to derive a SPIR-V version from a target environment
/// the method will choose the highest version of SPIR-V supported by the target
/// environment. Examples:
///    SPV_ENV_VULKAN_1_0           ->  SPIR-V 1.0
///    SPV_ENV_VULKAN_1_1           ->  SPIR-V 1.3
///    SPV_ENV_VULKAN_1_1_SPIRV_1_4 ->  SPIR-V 1.4
///    SPV_ENV_VULKAN_1_2           ->  SPIR-V 1.5
///
/// Consult the description of API entry points for specific rules.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(C)]
#[allow(non_camel_case_types)]
pub enum TargetEnv {
    /// SPIR-V 1.0 latest revision, no other restrictions.
    Universal_1_0,
    /// Vulkan 1.0 latest revision.
    Vulkan_1_0,
    /// SPIR-V 1.1 latest revision, no other restrictions.
    Universal_1_1,
    /// OpenCL Full Profile 2.1 latest revision.
    OpenCL_2_1,
    /// OpenCL Full Profile 2.2 latest revision.
    OpenCL_2_2,
    /// OpenGL 4.0 plus GL_ARB_gl_spirv, latest revisions.
    OpenGL_4_0,
    /// OpenGL 4.1 plus GL_ARB_gl_spirv, latest revisions.
    OpenGL_4_1,
    /// OpenGL 4.2 plus GL_ARB_gl_spirv, latest revisions.
    OpenGL_4_2,
    /// OpenGL 4.3 plus GL_ARB_gl_spirv, latest revisions.
    OpenGL_4_3,
    /// OpenGL 4.5 plus GL_ARB_gl_spirv, latest revisions.
    OpenGL_4_5,
    /// SPIR-V 1.2, latest revision, no other restrictions.
    Universal_1_2,
    /// OpenCL Full Profile 1.2 plus cl_khr_il_program, latest revision.
    OpenCL_1_2,
    /// OpenCL Embedded Profile 1.2 plus cl_khr_il_program, latest revision.
    OpenCLEmbedded_1_2,
    /// OpenCL Full Profile 2.0 plus cl_khr_il_program, latest revision.
    OpenCL_2_0,
    /// OpenCL Embedded Profile 2.0 plus cl_khr_il_program, latest revision.
    OpenCLEmbedded_2_0,
    /// OpenCL Embedded Profile 2.1 latest revision.
    OpenCLEmbedded_2_1,
    /// OpenCL Embedded Profile 2.2 latest revision.
    OpenCLEmbedded_2_2,
    /// SPIR-V 1.3 latest revision, no other restrictions.
    Universal_1_3,
    /// Vulkan 1.1 latest revision.
    Vulkan_1_1,
    /// Work in progress WebGPU 1.0.
    WebGPU_0,
    /// SPIR-V 1.4 latest revision, no other restrictions.
    Universal_1_4,
    /// Vulkan 1.1 with VK_KHR_spirv_1_4, i.e. SPIR-V 1.4 binary.
    Vulkan_1_1_Spirv_1_4,
    /// SPIR-V 1.5 latest revision, no other restrictions.
    Universal_1_5,
    /// Vulkan 1.2 latest revision.
    Vulkan_1_2,
}

impl Default for TargetEnv {
    fn default() -> Self {
        // This is the default target environment for (AFAICT) all spirv-tools
        Self::Universal_1_5
    }
}

impl std::str::FromStr for TargetEnv {
    type Err = SpirvResult;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "vulkan1.1spv1.4" => Self::Vulkan_1_1_Spirv_1_4,
            "vulkan1.0" => Self::Vulkan_1_0,
            "vulkan1.1" => Self::Vulkan_1_1,
            "vulkan1.2" => Self::Vulkan_1_2,
            "spv1.0" => Self::Universal_1_0,
            "spv1.1" => Self::Universal_1_1,
            "spv1.2" => Self::Universal_1_2,
            "spv1.3" => Self::Universal_1_3,
            "spv1.4" => Self::Universal_1_4,
            "spv1.5" => Self::Universal_1_5,
            "opencl1.2embedded" => Self::OpenCLEmbedded_1_2,
            "opencl1.2" => Self::OpenCL_1_2,
            "opencl2.0embedded" => Self::OpenCLEmbedded_2_0,
            "opencl2.0" => Self::OpenCL_2_0,
            "opencl2.1embedded" => Self::OpenCLEmbedded_2_1,
            "opencl2.1" => Self::OpenCL_2_1,
            "opencl2.2embedded" => Self::OpenCLEmbedded_2_2,
            "opencl2.2" => Self::OpenCL_2_2,
            "opengl4.0" => Self::OpenGL_4_0,
            "opengl4.1" => Self::OpenGL_4_1,
            "opengl4.2" => Self::OpenGL_4_2,
            "opengl4.3" => Self::OpenGL_4_3,
            "opengl4.5" => Self::OpenGL_4_5,
            "webgpu0" => Self::WebGPU_0,
            _ => return Err(SpirvResult::InvalidValue),
        })
    }
}

impl fmt::Display for TargetEnv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Vulkan_1_1_Spirv_1_4 => "vulkan1.1spv1.4",
            Self::Vulkan_1_0 => "vulkan1.0",
            Self::Vulkan_1_1 => "vulkan1.1",
            Self::Vulkan_1_2 => "vulkan1.2",
            Self::Universal_1_0 => "spv1.0",
            Self::Universal_1_1 => "spv1.1",
            Self::Universal_1_2 => "spv1.2",
            Self::Universal_1_3 => "spv1.3",
            Self::Universal_1_4 => "spv1.4",
            Self::Universal_1_5 => "spv1.5",
            Self::OpenCLEmbedded_1_2 => "opencl1.2embedded",
            Self::OpenCL_1_2 => "opencl1.2",
            Self::OpenCLEmbedded_2_0 => "opencl2.0embedded",
            Self::OpenCL_2_0 => "opencl2.0",
            Self::OpenCLEmbedded_2_1 => "opencl2.1embedded",
            Self::OpenCL_2_1 => "opencl2.1",
            Self::OpenCLEmbedded_2_2 => "opencl2.2embedded",
            Self::OpenCL_2_2 => "opencl2.2",
            Self::OpenGL_4_0 => "opengl4.0",
            Self::OpenGL_4_1 => "opengl4.1",
            Self::OpenGL_4_2 => "opengl4.2",
            Self::OpenGL_4_3 => "opengl4.3",
            Self::OpenGL_4_5 => "opengl4.5",
            Self::WebGPU_0 => "webgpu0",
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(i32)] // SPV_FORCE_32_BIT_ENUM
pub enum SpirvResult {
    Success = 0,
    Unsupported = 1,
    EndOfStream = 2,
    Warning = 3,
    FailedMatch = 4,
    /// Success, but signals early termination.
    RequestedTermination = 5,
    InternalError = -1,
    OutOfMemory = -2,
    InvalidPointer = -3,
    InvalidBinary = -4,
    InvalidText = -5,
    InvalidTable = -6,
    InvalidValue = -7,
    InvalidDiagnostic = -8,
    InvalidLookup = -9,
    InvalidId = -10,
    InvalidCfg = -11,
    InvalidLayout = -12,
    InvalidCapability = -13,
    /// Indicates data rules validation failure.
    InvalidData = -14,
    MissingExtension = -15,
    /// Indicates wrong SPIR-V version
    WrongVersion = -16,
}

impl fmt::Display for SpirvResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SpirvResult::*;

        match self {
            Success => f.write_str("success"),
            Unsupported => f.write_str("unsupported"),
            EndOfStream => f.write_str("end of stream"),
            Warning => f.write_str("warning"),
            FailedMatch => f.write_str("failed match"),
            RequestedTermination => f.write_str("requested termination"),
            InternalError => f.write_str("internal error"),
            OutOfMemory => f.write_str("out of memory"),
            InvalidPointer => f.write_str("invalid pointer"),
            InvalidBinary => f.write_str("invalid binary"),
            InvalidText => f.write_str("invalid text"),
            InvalidTable => f.write_str("invalid table"),
            InvalidValue => f.write_str("invalid value"),
            InvalidDiagnostic => f.write_str("invalid diagnostic"),
            InvalidLookup => f.write_str("invalid lookup"),
            InvalidId => f.write_str("invalid id"),
            InvalidCfg => f.write_str("invalid cfg"),
            InvalidLayout => f.write_str("invalid layout"),
            InvalidCapability => f.write_str("invalid capability"),
            InvalidData => f.write_str("invalid data"),
            MissingExtension => f.write_str("missing extension"),
            WrongVersion => f.write_str("wrong SPIR-V version"),
        }
    }
}

impl std::error::Error for SpirvResult {}

#[cfg(feature = "tools")]
#[repr(C)]
pub struct ToolContext {
    _unused: [u8; 0],
}

#[repr(C)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

#[repr(C)]
pub struct Diagnostic {
    pub position: Position,
    pub error: *const std::os::raw::c_char,
    pub is_text_source: bool,
}

#[cfg(feature = "tools")]
extern "C" {
    /// Creates a context object for most of the SPIRV-Tools API.
    /// Returns null if env is invalid.
    ///
    /// See specific API calls for how the target environment is interpeted
    /// (particularly assembly and validation).
    #[link_name = "spvContextCreate"]
    pub fn context_create(env: TargetEnv) -> *mut ToolContext;
    /// Destroys the given context object.
    #[link_name = "spvContextDestroy"]
    pub fn context_destroy(opt: *mut ToolContext);

    /// Destroys a diagnostic object.  This is a no-op if diagnostic is a null
    /// pointer.
    #[link_name = "spvDiagnosticDestroy"]
    pub fn diagnostic_destroy(diag: *mut Diagnostic);
}
