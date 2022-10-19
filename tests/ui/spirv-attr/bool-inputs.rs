// build-pass
// compile-flags: -Ctarget-feature=+FragmentFullyCoveredEXT,+ext:SPV_EXT_fragment_fully_covered

use spirv_std::spirv;

#[spirv(fragment)]
pub fn fragment(
    #[spirv(front_facing)] front_facing: bool,
    #[spirv(fully_covered_ext)] fully_covered_ext: bool,
    #[spirv(helper_invocation)] helper_invocation: bool,
) {
}
