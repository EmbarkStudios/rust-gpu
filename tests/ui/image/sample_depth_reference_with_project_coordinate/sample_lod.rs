// Test `OpImageSampleProjDrefExplicitLod`
// build-pass

use spirv_std::{arch, Image2d, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image2d,
    #[spirv(descriptor_set = 1, binding = 1)] sampler: &Sampler,
    output: &mut f32,
) {
    let v3 = glam::Vec3A::new(0.0, 0.0, 1.0);
    *output = image.sample_depth_reference_with_project_coordinate_by_lod(*sampler, v3, 1.0, 0.0);
}
