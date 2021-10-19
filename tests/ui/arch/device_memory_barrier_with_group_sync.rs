// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=device_memory_barrier_with_group_sync::device_memory_barrier_with_group_sync

use spirv_std as _;

unsafe fn device_memory_barrier_with_group_sync() {
    spirv_std::arch::device_memory_barrier_with_group_sync();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        device_memory_barrier_with_group_sync();
    }
}
