// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+RayQueryKHR,+ext:SPV_KHR_ray_tracing,+ext:SPV_KHR_ray_query

use glam::Vec3;
use spirv_std::ray_tracing::{AccelerationStructure, RayFlags, RayQuery};
use spirv_std::spirv;

#[spirv(fragment)]
// Rustfmt eats long attributes <https://github.com/rust-lang/rustfmt/issues/4579>
#[rustfmt::skip]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] acceleration_structure: &AccelerationStructure,
    #[spirv(ray_payload)] payload: &mut Vec3,
) {
    unsafe {
        spirv_std::ray_query!(let mut ray_query);

        ray_query.initialize(
            acceleration_structure,
            RayFlags::NONE,
            0,
            glam::vec3(1.0, 2.0, 3.0),
            0.5,
            glam::vec3(3.0, 2.0, 1.0),
            1.0,
        );
    }
}
