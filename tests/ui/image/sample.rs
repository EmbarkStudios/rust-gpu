// Test `OpImageSampleImplicitLod`
// build-pass

use spirv_std::spirv;
use spirv_std::{arch, Image, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image!(2D, type=f32, sampled),
    #[spirv(descriptor_set = 1, binding = 1)] image2d_array: &Image!(2D, type=f32, arrayed, sampled),
    #[spirv(descriptor_set = 2, binding = 2)] cubemap: &Image!(3D, type=f32, sampled),
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    let r1: glam::Vec4 = image2d.sample(*sampler, v2);
    let r2: glam::Vec4 = image2d_array.sample(*sampler, v3);
    let r3: glam::Vec4 = cubemap.sample(*sampler, v3);
    *output = r1 + r2 + r3;
}
