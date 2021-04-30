// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

#[spirv(any_hit)]
pub fn main() {
    unsafe {
        spirv_std::arch::ignore_intersection();
    }
}
