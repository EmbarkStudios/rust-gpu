// Test `OpImageSampleDrefImplicitLod`
// build-pass

use spirv_std::{arch, Cubemap, Image2d, Image2dArray, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(uniform_constant, descriptor_set = 0, binding = 0)] image: &Image2d,
    #[spirv(uniform_constant, descriptor_set = 1, binding = 1)] image_array: &Image2dArray,
    #[spirv(uniform_constant, descriptor_set = 2, binding = 2)] cubemap: &Cubemap,
    #[spirv(uniform_constant, descriptor_set = 3, binding = 3)] sampler: &Sampler,
    output: &mut f32,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3A::new(0.0, 0.0, 1.0);
    *output = image.sample_depth_reference(*sampler, v2, 1.0);
    *output += image_array.sample_depth_reference(*sampler, v3, 1.0);
    *output += cubemap.sample_depth_reference(*sampler, v3, 1.0);
}
