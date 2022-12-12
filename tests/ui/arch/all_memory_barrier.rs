// normalize-stderr-not_spirt "OpLine %8 11 1" -> "OpNoLine"

// build-pass
// compile-flags: -C target-feature=+VulkanMemoryModelDeviceScopeKHR,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=all_memory_barrier::all_memory_barrier

use spirv_std::spirv;

unsafe fn all_memory_barrier() {
    spirv_std::arch::all_memory_barrier();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        all_memory_barrier();
    }
}
