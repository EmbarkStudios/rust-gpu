// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::{arch, spirv, Image};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(rect, type=f32, sampled),
    output: &mut u32,
) {
    *output = image.query_levels();
}
