use spirv_std::spirv;

// Test `OpImageSampleProjImplicitLod`
// build-pass

use spirv_std::{Image, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image!(2D, type=f32, sampled),
    #[spirv(descriptor_set = 1, binding = 1)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    *output = image2d.sample_with_project_coordinate(*sampler, v3);
}
