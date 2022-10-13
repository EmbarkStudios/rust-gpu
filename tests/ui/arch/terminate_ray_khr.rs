// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

#[rust_gpu::spirv(any_hit)]
pub fn main() {
    unsafe {
        spirv_std::arch::terminate_ray();
    }
}
