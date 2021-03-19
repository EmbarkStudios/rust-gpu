// build-pass

use spirv_std::{arch, storage_class::{Output, UniformConstant}, Image2d};

#[spirv(fragment)]
pub fn main(image: UniformConstant<Image2d>, mut output: Output<glam::Vec4>) {
    let texel = image.fetch(glam::IVec2::new(0, 1));
    *output = texel;
}
