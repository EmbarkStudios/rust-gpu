// Test `OpImageSampleDrefExplicitLod`
// build-pass

use spirv_std::spirv;
use spirv_std::{Image, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled),
    #[spirv(descriptor_set = 1, binding = 1)] image_array: &Image!(2D, type=f32, arrayed, sampled),
    #[spirv(descriptor_set = 2, binding = 2)] sampler: &Sampler,
    output: &mut f32,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3A::new(0.0, 0.0, 1.0);
    *output = image.sample_depth_reference_by_lod(*sampler, v2, 1.0, 0.0);
    *output += image_array.sample_depth_reference_by_lod(*sampler, v3, 1.0, 0.0);
}

// NOTE(eddyb) this is separate because it runs afoul of this rule:
// > VUID-StandaloneSpirv-OpImage-04777
// > `OpImage*Dref*` instructions **must** not consume an image whose `Dim` is 3D
// FIXME(eddyb) look into whether non-Vulkan `OpImage*Dref*` usage can ever be 3D
#[cfg(not(any(
    target_env = "vulkan1.0",
    target_env = "vulkan1.1",
    target_env = "vulkan1.1spv1.4",
    target_env = "vulkan1.2"
)))]
#[spirv(fragment)]
pub fn main_cubemap(
    #[spirv(descriptor_set = 2, binding = 2)] sampler: &Sampler,
    #[spirv(descriptor_set = 3, binding = 3)] cubemap: &Image!(3D, type=f32, sampled),
    output: &mut f32,
) {
    let v3 = glam::Vec3A::new(0.0, 0.0, 1.0);
    *output += cubemap.sample_depth_reference_by_lod(*sampler, v3, 1.0, 0.0);
}
