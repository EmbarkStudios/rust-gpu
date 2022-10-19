// build-fail

use spirv_std::spirv;
use spirv_std::{arch, Image, Sampler};

fn deeper_stack(image2d: &Image!(2D, type=f32, sampled), sampler: &Sampler) -> glam::Vec4 {
    let v2 = glam::Vec2::new(0.0, 1.0);
    image2d.sample(*sampler, v2)
}
fn deep_stack(image2d: &Image!(2D, type=f32, sampled), sampler: &Sampler) -> glam::Vec4 {
    deeper_stack(image2d, sampler)
}

#[spirv(vertex)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image!(2D, type=f32, sampled),
    #[spirv(descriptor_set = 0, binding = 1)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let r0 = deep_stack(image2d, sampler);
    let r1: glam::Vec4 = image2d.sample(*sampler, v2);
    *output = r0 + r1;
}
