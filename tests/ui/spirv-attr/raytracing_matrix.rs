// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std as _;

#[spirv(closest_hit)]
pub fn main(
    #[spirv(object_to_world)] _object_to_world: [glam::Vec3; 4],
    #[spirv(world_to_object)] _world_to_object: [glam::Vec3; 4],
) {
}
