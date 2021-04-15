// build-fail

use spirv_std::{Image2d, RuntimeArray};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] one: &[Image2d],
    #[spirv(uniform, descriptor_set = 0, binding = 0)] two: &RuntimeArray<u32>,
) {
}
