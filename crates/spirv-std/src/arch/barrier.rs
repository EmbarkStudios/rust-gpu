use crate::memory::{Scope, Semantics};

/// Wait for other invocations of this module to reach the current point
/// of execution.
///
/// All invocations of this module within Execution scope reach this point of
/// execution before any invocation proceeds beyond it.
///
/// When Execution is [`Scope::Workgroup`] or larger, behavior is undefined
/// unless all invocations within Execution execute the same dynamic instance of
/// this instruction. When Execution is Subgroup or Invocation, the behavior of
/// this instruction in non-uniform control flow is defined by the client API.
///
/// If [`Semantics`] is not [`Semantics::None`], this instruction also serves as
/// an [`memory_barrier`] function call, and also performs and adheres to the
/// description and semantics of an [`memory_barrier`] function with the same
/// `MEMORY` and `SEMANTICS` operands. This allows atomically specifying both a
/// control barrier and a memory barrier (that is, without needing two
/// instructions). If [`Semantics`] is [`Semantics::None`], `MEMORY` is ignored.
///
/// Before SPIRV-V version 1.3, it is only valid to use this instruction with
/// `TessellationControl`, `GLCompute`, or `Kernel` execution models. There is
/// no such restriction starting with version 1.3.
///
/// If used with the `TessellationControl` execution model, it also implicitly
/// synchronizes the [`crate::storage_class::Output`] Storage Class: Writes to
/// `Output` variables performed by any invocation executed prior to a
/// [`control_barrier`] are visible to any other invocation proceeding beyond
/// that [`control_barrier`].
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpControlBarrier")]
#[inline]
pub unsafe fn control_barrier<
    const EXECUTION: Scope,
    const MEMORY: Scope,
    const SEMANTICS: Semantics,
>() {
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%execution = OpConstant %u32 {execution}",
        "%memory = OpConstant %u32 {memory}",
        "%semantics = OpConstant %u32 {semantics}",
        "OpControlBarrier %execution %memory %semantics",
        execution = const EXECUTION as u8,
        memory = const MEMORY as u8,
        semantics = const SEMANTICS as u8,
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
pub unsafe fn memory_barrier<const MEMORY: Scope, const SEMANTICS: Semantics>() {
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%memory = OpConstant %u32 {memory}",
        "%semantics = OpConstant %u32 {semantics}",
        "OpMemoryBarrier %memory %semantics",
        memory = const MEMORY as u8,
        semantics = const SEMANTICS as u8,
    }
}
