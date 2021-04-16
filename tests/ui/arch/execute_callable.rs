// build-pass

#[spirv(ray_generation)]
// Rustfmt will eat long attributes (https://github.com/rust-lang/rustfmt/issues/4579)
#[rustfmt::skip]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)]
    acceleration_structure: &spirv_std::ray_tracing::AccelerationStructure,
    #[spirv(incoming_callable_data)] payload: &glam::Vec3,
) {
    unsafe {
        asm!(r#"OpExtension "SPV_KHR_ray_tracing""#);
        asm!("OpCapability RayTracingKHR");
        spirv_std::arch::execute_callable::<_, 5>(payload);
    }
}
