// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+RayQueryKHR,+ext:SPV_KHR_ray_tracing,+ext:SPV_KHR_ray_query

#[cfg(not(target_arch = "spirv"))]
use spirv_std::macros::spirv;
use spirv_std::{glam::Vec3, ray_query};
use spirv_std::ray_tracing::{AccelerationStructure, CommittedIntersection, RayFlags, RayQuery};
use spirv_std::arch::IndexUnchecked;

pub fn some_fun(mut c: impl FnMut(u32) -> bool) {
    let mut i = 0;
    while i < 10 {
        if c(i) {
            return;
        }
        i += 1;
    }
}
#[spirv(intersection)]
pub fn voxels_gbuffer_is(
    #[spirv(storage_buffer, descriptor_set = 1, binding = 1)] vertices: &[u32],
    #[spirv(descriptor_set = 1, binding = 3)] acceleration_structure: &AccelerationStructure,
) {
    some_fun(|i| {
        unsafe {
            ray_query!(let mut handle);
            handle.initialize(
                acceleration_structure,
                RayFlags::TERMINATE_ON_FIRST_HIT,
                0xFF,
                Vec3::new(0.0, 0.0, 0.0),
                0.0,
                Vec3::new(0.0, 0.0, 0.0),
                10000.0,
            );
            let ind = unsafe {
                vertices.index_unchecked(0)
            };
            ind + i == 2
        }
    });
}

