// Test `OpImageSampleExplicitLod`
// build-pass

use spirv_std::{arch, storage_class::{Output, UniformConstant}, Image2d, Image2dArray, Sampler};

#[spirv(fragment)]
pub fn main(
    image: UniformConstant<Image2d>,
    image_array: UniformConstant<Image2dArray>,
    sampler: UniformConstant<Sampler>,
    mut image_output: Output<glam::Vec4>,
    mut image_array_output: Output<glam::Vec4>,
) {
    let image_result = image.sample_by_lod(*sampler, glam::Vec2::new(0.0, 1.0), 0.0);
    *image_output = image_result;
    let image_array_result = image_array.sample_by_lod(*sampler, glam::Vec3A::new(0.0, 0.0, 1.0), 0.0);
    *image_array_output = image_array_result;
}
