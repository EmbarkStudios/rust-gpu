// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

#[spirv(intersection)]
pub fn main() {
    unsafe {
        spirv_std::arch::report_intersection(2.0, 4);
    }
}
