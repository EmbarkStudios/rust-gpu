// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_non_uniform_ballot::subgroup_non_uniform_ballot

use spirv_std::spirv;

unsafe fn subgroup_non_uniform_ballot(predicate: bool) -> bool {
    let ballot = spirv_std::arch::subgroup_non_uniform_ballot(predicate);
    spirv_std::arch::subgroup_non_uniform_inverse_ballot(ballot)
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        subgroup_non_uniform_ballot(true);
    }
}
