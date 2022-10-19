// Test `OpImageGather`
// build-pass

use core::arch::asm;
use spirv_std::spirv;
use spirv_std::{arch, Image, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image!(2D, type=f32, sampled),
    #[spirv(descriptor_set = 2, binding = 2)] cubemap: &Image!(cube, type=f32, sampled),
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    let r1: glam::Vec4 = image2d.gather(*sampler, v2, 0);
    let r2: glam::Vec4 = cubemap.gather(*sampler, v3, 0);
    *output = r1 + r2;
}

#[cfg(not(any(
    target_env = "vulkan1.0",
    target_env = "vulkan1.1",
    target_env = "vulkan1.1spv1.4",
    target_env = "vulkan1.2"
)))]
#[spirv(fragment)]
pub fn main_rect(
    #[spirv(descriptor_set = 1, binding = 1)] rect: &Image!(rect, type=f32, sampled),
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    // Must be asm! and not -Ctarget-feature=+SampledRect due to being in cfg
    unsafe { asm!("OpCapability SampledRect") };
    let v2 = glam::Vec2::new(0.0, 1.0);
    *output = rect.gather(*sampler, v2, 0);
}
