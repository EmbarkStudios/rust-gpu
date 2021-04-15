// build-pass
use spirv_std::{Image2d, RuntimeArray, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] sampler: &Sampler,
    #[spirv(descriptor_set = 0, binding = 1)] slice: &RuntimeArray<Image2d>,
    #[spirv(descriptor_set = 0, binding = 2)] sized_slice: &[Image2d; 5],
    output: &mut glam::Vec4,
) {
    unsafe {
        asm!(
            "OpCapability RuntimeDescriptorArray",
            "OpExtension \"SPV_EXT_descriptor_indexing\""
        )
    }
    let img = unsafe { slice.index(5) };
    let v2 = glam::Vec2::new(0.0, 1.0);
    *output = img.sample(*sampler, v2);

    let img_2 = &sized_slice[2];
    *output += img_2.sample(*sampler, v2);
}
