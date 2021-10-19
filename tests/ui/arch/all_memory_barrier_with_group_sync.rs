// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=all_memory_barrier_with_group_sync::all_memory_barrier_with_group_sync

use spirv_std as _;

unsafe fn all_memory_barrier_with_group_sync() {
    spirv_std::arch::all_memory_barrier_with_group_sync();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        all_memory_barrier_with_group_sync();
    }
}
