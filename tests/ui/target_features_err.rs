// build-fail
// compile-flags: -Ctarget-feature=+rayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std as _;

#[spirv(any_hit)]
pub fn main() {
    unsafe { spirv_std::arch::terminate_ray() }
}

