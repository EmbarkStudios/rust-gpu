// Test `OpImageSampleImplicitLod`
// build-pass

use spirv_std::{arch, Cubemap, Image2d, Image2dArray, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image2d,
    #[spirv(descriptor_set = 1, binding = 1)] image2d_array: &Image2dArray,
    #[spirv(descriptor_set = 2, binding = 2)] cubemap: &Cubemap,
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    *output = image2d.sample(*sampler, v2);
    *output += image2d_array.sample(*sampler, v3);
    *output += cubemap.sample(*sampler, v3);
}
