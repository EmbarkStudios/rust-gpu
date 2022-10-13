// Test `OpImageSampleProjExplicitLod`
// build-pass

use spirv_std::{arch, Image, Sampler};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image2d: &Image!(2D, type=f32, sampled),
    #[rust_gpu::spirv(descriptor_set = 1, binding = 1)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    *output = image2d.sample_with_project_coordinate_by_lod(*sampler, v3, 0.0);
}
