// build-pass
// compile-flags: -Ctarget-feature=+VariablePointers

use spirv_std as _;

use core::num::NonZeroU32;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] option_a: Option<NonZeroU32>, #[spirv(flat)] output: &mut u32) {
    *output = option_a.map_or(u32::MAX, NonZeroU32::get);
}
