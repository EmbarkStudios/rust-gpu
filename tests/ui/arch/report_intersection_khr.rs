// build-pass

#[spirv(intersection)]
pub fn main() {
    unsafe {
        asm!(r#"OpExtension "SPV_KHR_ray_tracing""#);
        asm!("OpCapability RayTracingKHR");
        spirv_std::arch::report_intersection(2.0, 4);
    }
}
