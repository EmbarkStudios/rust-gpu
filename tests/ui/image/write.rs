// Test `OpImageWrite`
// build-pass
// compile-flags: -C target-feature=+StorageImageWriteWithoutFormat

use spirv_std::{arch, Image};

#[rust_gpu::spirv(fragment)]
pub fn main(
    texels: glam::Vec2,
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled=false),
) {
    unsafe {
        image.write(glam::UVec2::new(0, 1), texels);
    }
}
