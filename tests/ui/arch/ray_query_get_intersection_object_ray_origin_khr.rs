// build-pass
// compile-flags: -Ctarget-feature=+RayQueryKHR,+ext:SPV_KHR_ray_query

use glam::Vec3;
use spirv_std::ray_tracing::{AccelerationStructure, RayFlags, RayQuery};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(descriptor_set = 0, binding = 0)] accel: &AccelerationStructure) {
    unsafe {
        spirv_std::ray_query!(let mut handle);
        handle.initialize(accel, RayFlags::NONE, 0, Vec3::ZERO, 0.0, Vec3::ZERO, 0.0);
        let origin: glam::Vec3 = handle.get_candidate_intersection_object_ray_origin();
        let origin: glam::Vec3 = handle.get_committed_intersection_object_ray_origin();
    }
}
