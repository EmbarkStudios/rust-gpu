// build-fail

use spirv_std::{spirv, Image, RuntimeArray};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] one: &[Image!(2D, type=f32, sampled)],
    #[spirv(uniform, descriptor_set = 0, binding = 0)] two: &RuntimeArray<u32>,
) {
}
