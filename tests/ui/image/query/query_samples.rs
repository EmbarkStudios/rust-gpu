// build-pass

use spirv_std::{arch, Image};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled, multisampled),
    output: &mut u32,
) {
    unsafe { asm!("OpCapability ImageQuery") };
    *output = image.query_samples();
}
