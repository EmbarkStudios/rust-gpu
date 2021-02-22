// Test `OpImageWrite`
// build-pass

use spirv_std::{arch, StorageImage2d};

#[spirv(fragment)]
pub fn main(input: &glam::Vec2, #[spirv(uniform_constant)] image: &StorageImage2d) {
    let texels = *input;
    unsafe {
        image.write(glam::UVec2::new(0, 1), texels);
    }
}
