// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std::spirv;

#[spirv(ray_generation)]
// Rustfmt will eat long attributes (https://github.com/rust-lang/rustfmt/issues/4579)
#[rustfmt::skip]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)]
    acceleration_structure: &spirv_std::ray_tracing::AccelerationStructure,
    #[spirv(ray_payload)] payload: &mut glam::Vec3,
) {
    unsafe {
        acceleration_structure.trace_ray(
            spirv_std::ray_tracing::RayFlags::NONE,
            0,
            0,
            0,
            0,
            glam::vec3(1.0, 2.0, 3.0),
            0.5,
            glam::vec3(3.0, 2.0, 1.0),
            1.0,
            payload,
        );
    }
}
