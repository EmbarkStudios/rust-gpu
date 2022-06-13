#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Wait for other invocations of this module to reach the current point
/// of execution.
///
/// All invocations of this module within Execution scope reach this point of
/// execution before any invocation proceeds beyond it.
///
/// When Execution is [`crate::memory::Scope::Workgroup`] or larger, behavior is
/// undefined unless all invocations within Execution execute the same dynamic
/// instance of this instruction. When Execution is Subgroup or Invocation, the
/// behavior of this instruction in non-uniform control flow is defined by the
/// client API.
///
/// If [`crate::memory::Semantics`] is not [`crate::memory::Semantics::NONE`],
/// this instruction also serves as an [`memory_barrier`] function call, and
/// also performs and adheres to the description and semantics of an
/// [`memory_barrier`] function with the same `MEMORY` and `SEMANTICS` operands.
/// This allows atomically specifying both a control barrier and a memory
/// barrier (that is, without needing two instructions). If
/// [`crate::memory::Semantics`] is [`crate::memory::Semantics::NONE`], `MEMORY`
/// is ignored.
///
/// Before SPIRV-V version 1.3, it is only valid to use this instruction with
/// `TessellationControl`, `GLCompute`, or `Kernel` execution models. There is
/// no such restriction starting with version 1.3.
///
/// If used with the `TessellationControl` execution model, it also implicitly
/// synchronizes the `output` storage class: Writes to `output` variables
/// performed by any invocation executed prior to a [`control_barrier`] are
/// visible to any other invocation proceeding beyond that [`control_barrier`].
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpControlBarrier")]
#[inline]
pub unsafe fn control_barrier<
    const EXECUTION: u32, // Scope
    const MEMORY: u32,    // Scope
    const SEMANTICS: u32, // Semantics
>() {
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%execution = OpConstant %u32 {execution}",
        "%memory = OpConstant %u32 {memory}",
        "%semantics = OpConstant %u32 {semantics}",
        "OpControlBarrier %execution %memory %semantics",
        execution = const EXECUTION,
        memory = const MEMORY,
        semantics = const SEMANTICS,
    }
}

/// Control the order that memory accesses are observed.
///
/// Ensures that memory accesses issued before this instruction are observed
/// before memory accesses issued after this instruction. This control is
/// ensured only for memory accesses issued by this invocation and observed by
/// another invocation executing within `MEMORY` scope. If the `vulkan` memory
/// model is declared, this ordering only applies to memory accesses that
/// use the `NonPrivatePointer` memory operand or `NonPrivateTexel`
/// image operand.
///
/// `SEMANTICS` declares what kind of memory is being controlled and what kind
/// of control to apply.
///
/// To execute both a memory barrier and a control barrier,
/// see [`control_barrier`].
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpMemoryBarrier")]
#[inline]
pub unsafe fn memory_barrier<
    const MEMORY: u32,    // Scope
    const SEMANTICS: u32, // Semantics
>() {
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%memory = OpConstant %u32 {memory}",
        "%semantics = OpConstant %u32 {semantics}",
        "OpMemoryBarrier %memory %semantics",
        memory = const MEMORY,
        semantics = const SEMANTICS,
    }
}

/// Blocks execution of all threads in a group until all group shared accesses have been completed.
///
/// This is an exact implementation of `GroupMemoryBarrier()`.
///
/// From <https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/groupmemorybarrier>
#[spirv_std_macros::gpu_only]
#[inline]
pub unsafe fn workgroup_memory_barrier() {
    memory_barrier::<
        { crate::memory::Scope::Workgroup as u32 },
        {
            crate::memory::Semantics::WORKGROUP_MEMORY.bits()
                | crate::memory::Semantics::ACQUIRE_RELEASE.bits()
        },
    >();
}

/// Blocks execution of all threads in a group until all group shared accesses have been completed and all threads in the group have reached this call.
///
/// This is an exact implementation of `GroupMemoryBarrierWithGroupSync()`.
///
/// From <https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/groupmemorybarrierwithgroupsync>
#[spirv_std_macros::gpu_only]
#[inline]
pub unsafe fn workgroup_memory_barrier_with_group_sync() {
    control_barrier::<
        { crate::memory::Scope::Workgroup as u32 },
        { crate::memory::Scope::Workgroup as u32 },
        {
            crate::memory::Semantics::WORKGROUP_MEMORY.bits()
                | crate::memory::Semantics::ACQUIRE_RELEASE.bits()
        },
    >();
}

/// Blocks execution of all threads in a group until all device memory accesses have been completed.
///
/// This is an exact implementation of `DeviceMemoryBarrier()`.
///
/// From <https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/devicememorybarrier>
#[spirv_std_macros::gpu_only]
#[inline]
pub unsafe fn device_memory_barrier() {
    memory_barrier::<
        { crate::memory::Scope::Device as u32 },
        {
            crate::memory::Semantics::IMAGE_MEMORY.bits()
                | crate::memory::Semantics::UNIFORM_MEMORY.bits()
                | crate::memory::Semantics::ACQUIRE_RELEASE.bits()
        },
    >();
}

/// Blocks execution of all threads in a group until all device memory accesses have been completed and all threads in the group have reached this call.
///
/// This is an exact implementation of `DeviceMemoryBarrierWithGroupSync()`.
///
/// From <https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/devicememorybarrierwithgroupsync>
#[spirv_std_macros::gpu_only]
#[inline]
pub unsafe fn device_memory_barrier_with_group_sync() {
    control_barrier::<
        { crate::memory::Scope::Workgroup as u32 },
        { crate::memory::Scope::Device as u32 },
        {
            crate::memory::Semantics::IMAGE_MEMORY.bits()
                | crate::memory::Semantics::UNIFORM_MEMORY.bits()
                | crate::memory::Semantics::ACQUIRE_RELEASE.bits()
        },
    >();
}

/// Blocks execution of all threads in a group until all memory accesses have been completed.
///
/// This is an exact implementation of `AllMemoryBarrier()`.
///
/// From <https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/allmemorybarrier>
#[spirv_std_macros::gpu_only]
#[inline]
pub unsafe fn all_memory_barrier() {
    memory_barrier::<
        { crate::memory::Scope::Device as u32 },
        {
            crate::memory::Semantics::WORKGROUP_MEMORY.bits()
                | crate::memory::Semantics::IMAGE_MEMORY.bits()
                | crate::memory::Semantics::UNIFORM_MEMORY.bits()
                | crate::memory::Semantics::ACQUIRE_RELEASE.bits()
        },
    >();
}

/// Blocks execution of all threads in a group until all memory accesses have been completed and all threads in the group have reached this call.
///
/// This is an exact implementation of `AllMemoryBarrierWithGroupSync()`.
///
/// From <https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/allmemorybarrierwithgroupsync>
#[spirv_std_macros::gpu_only]
#[inline]
pub unsafe fn all_memory_barrier_with_group_sync() {
    control_barrier::<
        { crate::memory::Scope::Workgroup as u32 },
        { crate::memory::Scope::Device as u32 },
        {
            crate::memory::Semantics::WORKGROUP_MEMORY.bits()
                | crate::memory::Semantics::IMAGE_MEMORY.bits()
                | crate::memory::Semantics::UNIFORM_MEMORY.bits()
                | crate::memory::Semantics::ACQUIRE_RELEASE.bits()
        },
    >();
}
