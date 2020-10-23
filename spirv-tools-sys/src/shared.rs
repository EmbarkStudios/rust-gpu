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
#[derive(Copy, Clone, Debug)]
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

#[cfg(feature = "tools")]
#[repr(C)]
pub struct Tool {
    _unused: [u8; 0],
}

#[cfg(feature = "tools")]
extern "C" {
    pub fn tool_create(env: TargetEnv) -> *mut Tool;
    pub fn tool_destroy(opt: *mut Tool);
}
