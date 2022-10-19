// build-pass
//
// compile-flags: -C target-feature=+DemoteToHelperInvocationEXT,+ext:SPV_EXT_demote_to_helper_invocation

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    unsafe { spirv_std::arch::demote_to_helper_invocation() };
    assert!(spirv_std::arch::is_helper_invocation());
}
