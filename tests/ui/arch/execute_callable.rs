// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std::spirv;

#[spirv(ray_generation)]
// Rustfmt will eat long attributes (https://github.com/rust-lang/rustfmt/issues/4579)
#[rustfmt::skip]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)]
    acceleration_structure: &spirv_std::ray_tracing::AccelerationStructure,
    #[spirv(callable_data)] payload: &glam::Vec3,
) {
    unsafe {
        spirv_std::arch::execute_callable::<_, 5>(payload);
    }
}
