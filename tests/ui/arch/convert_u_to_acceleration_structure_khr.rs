// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std::spirv;

#[spirv(ray_generation)]
pub fn main(#[spirv(ray_payload)] payload: &mut glam::Vec3) {
    unsafe {
        let handle = spirv_std::ray_tracing::AccelerationStructure::from_u64(0xffff_ffff);
        let handle2 =
            spirv_std::ray_tracing::AccelerationStructure::from_vec(glam::UVec2::new(0, 0));

        handle.trace_ray(
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

        handle2.trace_ray(
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
