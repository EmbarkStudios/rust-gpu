// build-pass

use spirv_std::{arch, Image2d};

#[spirv(fragment)]
pub fn main(#[spirv(uniform_constant)] image: &Image2d, output: &mut glam::Vec4) {
    let texel = image.fetch(glam::IVec2::new(0, 1));
    *output = texel;
}
