// Test `OpImageSampleExplicitLod` Grad
// build-pass

use spirv_std::{
    arch,
    storage_class::{Output, UniformConstant},
    Cubemap, Image2d, Image2dArray, Sampler,
};

#[spirv(fragment)]
pub fn main(
    image2d: UniformConstant<Image2d>,
    image2d_array: UniformConstant<Image2dArray>,
    cubemap: UniformConstant<Cubemap>,
    sampler: UniformConstant<Sampler>,
    mut output: Output<glam::Vec4>,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    *output = image2d.sample_by_gradient(*sampler, v2, v2, v2);
    *output += image2d_array.sample_by_gradient(*sampler, v3, v2, v2);
    *output += cubemap.sample_by_gradient(*sampler, v3, v3, v3);
}
