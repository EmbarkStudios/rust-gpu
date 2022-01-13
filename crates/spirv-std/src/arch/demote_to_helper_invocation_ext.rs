#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Demote fragment shader invocation to a helper invocation. Equivalvent to
/// `discard()` in HLSL. Any stores to memory after this instruction are
/// suppressed and the fragment does not write outputs to the framebuffer.
///
/// Unlike [super::kill], this does not necessarily terminate the invocation. It
/// is not considered a flow control instruction (flow control does not become
/// non-uniform) and does not terminate the block.
///
/// - **Required Capabilities** `DemoteToHelperInvocationEXT`
/// - **Required Extensions** `SPV_EXT_demote_to_helper_invocation`
///
/// # Safety
/// After this instruction executes, the value of a `helper_invocation` builtin
/// variable is undefined. Use `is_helper_invocation` to determine whether
/// invocations are helper invocations in the presence
/// of [demote_to_helper_invocation].
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpDemoteToHelperInvocationEXT", alias = "discard")]
pub unsafe fn demote_to_helper_invocation() {
    asm!("OpDemoteToHelperInvocationEXT");
}

/// Returns `true` if the invocation is currently a helper invocation, otherwise
/// result is `false`. An invocation is currently a helper invocation if it was
/// originally invoked as a helper invocation or if it has been demoted to a
/// helper invocation by [demote_to_helper_invocation].
///
/// - **Required Capabilities** `DemoteToHelperInvocationEXT`
/// - **Required Extensions** `SPV_EXT_demote_to_helper_invocation`
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpIsHelperInvocationEXT")]
pub fn is_helper_invocation() -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%result = OpIsHelperInvocationEXT %bool",
            "OpStore {result} %result",
            result = in(reg) &mut result,
        };
    }

    result
}
