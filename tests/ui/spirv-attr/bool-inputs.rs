// build-pass
// compile-flags: -Ctarget-feature=+FragmentFullyCoveredEXT,+ext:SPV_EXT_fragment_fully_covered

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn fragment(
    #[rust_gpu::spirv(front_facing)] front_facing: bool,
    #[rust_gpu::spirv(fully_covered_ext)] fully_covered_ext: bool,
    #[rust_gpu::spirv(helper_invocation)] helper_invocation: bool,
) {
}
