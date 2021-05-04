// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::{arch, Image};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled),
    output: &mut glam::UVec2,
) {
    *output = image.query_size();
}
