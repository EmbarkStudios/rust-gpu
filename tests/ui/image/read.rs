// Test `OpImageRead`
// build-pass

use spirv_std::{arch, StorageImage2d};

#[spirv(fragment)]
pub fn main(#[spirv(uniform_constant)] image: &StorageImage2d, output: &mut glam::Vec2) {
    let coords = image.read(glam::IVec2::new(0, 1));
    *output = coords;
}
