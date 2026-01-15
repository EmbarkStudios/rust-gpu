// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_non_uniform_broadcast_first::subgroup_non_uniform_broadcast_first

use glam::Vec3;
use spirv_std::spirv;

unsafe fn subgroup_non_uniform_broadcast_first(vec: Vec3) -> Vec3 {
    spirv_std::arch::subgroup_non_uniform_broadcast_first::<Vec3>(vec)
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        subgroup_non_uniform_broadcast_first(Vec3::new(1., 2., 3.));
    }
}
