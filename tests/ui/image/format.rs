// build-pass

use spirv_std::{arch, Image};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, format=rgba32f, sampled),
    output: &mut glam::Vec4,
) {
    let texel = image.fetch(glam::IVec2::new(0, 1));
    *output = texel;
}
