// normalize-stderr-not_spirt "OpLine %8 10 1" -> "OpNoLine"

// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=workgroup_memory_barrier_with_group_sync::workgroup_memory_barrier_with_group_sync

use spirv_std::spirv;

unsafe fn workgroup_memory_barrier_with_group_sync() {
    spirv_std::arch::workgroup_memory_barrier_with_group_sync();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        workgroup_memory_barrier_with_group_sync();
    }
}
