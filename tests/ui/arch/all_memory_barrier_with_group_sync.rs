// build-pass
// compile-flags: -C target-feature=+VulkanMemoryModelDeviceScopeKHR,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=all_memory_barrier_with_group_sync::all_memory_barrier_with_group_sync

use spirv_std as _;

unsafe fn all_memory_barrier_with_group_sync() {
    spirv_std::arch::all_memory_barrier_with_group_sync();
}

#[rust_gpu::spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        all_memory_barrier_with_group_sync();
    }
}
