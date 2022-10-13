// Test `OpImageRead`
// build-pass
// compile-flags: -C target-feature=+StorageImageReadWithoutFormat

use spirv_std::{arch, Image};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled=false),
    output: &mut glam::Vec4,
) {
    let coords = image.read(glam::IVec2::new(0, 1));
    *output = coords;
}
