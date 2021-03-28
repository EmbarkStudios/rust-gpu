// Test `OpImageSampleDrefExplicitLod`
// build-pass

use spirv_std::{arch, Cubemap, Image2d, Image2dArray, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(uniform_constant, descriptor_set = 0, binding = 0)] image: &Image2d,
    #[spirv(uniform_constant, descriptor_set = 1, binding = 1)] image_array: &Image2dArray,
    #[spirv(uniform_constant, descriptor_set = 2, binding = 2)] sampler: &Sampler,
    #[spirv(uniform_constant, descriptor_set = 3, binding = 3)] cubemap: &Cubemap,
    output: &mut f32,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v2_dx = glam::Vec2::new(0.0, 1.0);
    let v2_dy = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3A::new(0.0, 0.0, 1.0);
    let v3_dx = glam::Vec3A::new(0.0, 1.0, 0.5);
    let v3_dy = glam::Vec3A::new(0.0, 1.0, 0.5);
    *output = image.sample_depth_reference_by_gradient(*sampler, v2, 1.0, v2_dx, v2_dy);
    *output += image_array.sample_depth_reference_by_gradient(*sampler, v3, 1.0, v2_dx, v2_dy);
    *output += cubemap.sample_depth_reference_by_gradient(*sampler, v3, 1.0, v3_dx, v3_dy);
}
