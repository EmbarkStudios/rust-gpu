// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+RayQueryKHR,+ext:SPV_KHR_ray_tracing,+ext:SPV_KHR_ray_query

#[cfg(not(target_arch = "spirv"))]
use spirv_std::macros::spirv;
use spirv_std::ray_tracing::{AccelerationStructure, RayFlags, RayQuery};
use spirv_std::{glam::Vec3, ray_query};

#[inline]
pub fn some_fun(mut c: impl FnMut() -> bool) {
    c();
}
#[spirv(intersection)]
pub fn voxels_gbuffer_is(
    #[spirv(descriptor_set = 1, binding = 3)] acceleration_structure: &AccelerationStructure,
) {
    ray_query!(let mut handle);
    some_fun(|| {
        unsafe {
            handle.initialize(
                acceleration_structure,
                RayFlags::TERMINATE_ON_FIRST_HIT,
                0xFF,
                Vec3::new(0.0, 0.0, 0.0),
                0.0,
                Vec3::new(0.0, 0.0, 0.0),
                10000.0,
            );
            true
        }
    });
}
