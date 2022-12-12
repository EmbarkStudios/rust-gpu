// normalize-stderr-not_spirt "OpLine %9 11 1" -> "OpNoLine"

// build-pass
// compile-flags: -C target-feature=+VulkanMemoryModelDeviceScopeKHR,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=device_memory_barrier_with_group_sync::device_memory_barrier_with_group_sync

use spirv_std::spirv;

unsafe fn device_memory_barrier_with_group_sync() {
    spirv_std::arch::device_memory_barrier_with_group_sync();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        device_memory_barrier_with_group_sync();
    }
}
