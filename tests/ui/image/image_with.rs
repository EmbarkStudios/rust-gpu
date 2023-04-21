// build-pass

use spirv_std::spirv;
use spirv_std::{arch, image::sample_with, image::ImageWithMethods, Image, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] sampler: &Sampler,
    #[spirv(descriptor_set = 0, binding = 1)] image1: &Image!(2D, type=f32, sampled, multisampled),
    #[spirv(descriptor_set = 0, binding = 2)] image2: &Image!(2D, type=f32, sampled),
    output: &mut glam::Vec4,
) {
    let t1 = image1.fetch_with(glam::IVec2::new(0, 0), sample_with::sample_index(1));
    let t2 = image2.sample_with(*sampler, glam::Vec2::new(0.5, 0.5), sample_with::bias(1.0));
    let t3 = image2.sample_with(*sampler, glam::Vec2::new(0.5, 0.5), sample_with::lod(2.0));
    let t4 = image2.sample_with(
        *sampler,
        glam::Vec2::new(0.5, 0.5),
        sample_with::grad(glam::Vec2::new(0.5, 0.5), glam::Vec2::new(0.5, 0.5)),
    );
    *output = t1 + t2 + t3 + t4;
}
