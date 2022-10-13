// build-fail

use spirv_std::{Image, RuntimeArray};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] one: &[Image!(2D, type=f32, sampled)],
    #[rust_gpu::spirv(uniform, descriptor_set = 0, binding = 0)] two: &RuntimeArray<u32>,
) {
}
