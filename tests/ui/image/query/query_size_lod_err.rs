// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::{arch, Image};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image: &Image!(rect, type=f32, sampled),
    output: &mut glam::UVec2,
) {
    *output = image.query_size_lod(0);
}
