// build-fail

use spirv_std::{arch, Image, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(rect, type=f32, sampled),
    #[spirv(descriptor_set = 0, binding = 1)] sampler: &Sampler,
    output: &mut glam::Vec2,
) {
    unsafe { asm!("OpCapability ImageQuery") };
    *output = image.query_lod(*sampler, glam::Vec2::new(0.0, 1.0));
}
