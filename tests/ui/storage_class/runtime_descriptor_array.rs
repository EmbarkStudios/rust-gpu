// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing

use spirv_std::{Image, RuntimeArray, Sampler};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] sampler: &Sampler,
    #[rust_gpu::spirv(descriptor_set = 0, binding = 1)] slice: &RuntimeArray<
        Image!(2D, type=f32, sampled),
    >,
    #[rust_gpu::spirv(descriptor_set = 0, binding = 2)] sized_slice: &[Image!(2D, type=f32, sampled);
         5],
    output: &mut glam::Vec4,
) {
    let img = unsafe { slice.index(5) };
    let v2 = glam::Vec2::new(0.0, 1.0);
    let r1: glam::Vec4 = img.sample(*sampler, v2);

    let img_2 = &sized_slice[2];
    let r2: glam::Vec4 = img_2.sample(*sampler, v2);

    *output = r1 + r2;
}
