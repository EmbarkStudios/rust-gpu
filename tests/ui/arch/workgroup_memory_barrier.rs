// normalize-stderr-not_spirt "OpLine %8 10 1" -> "OpNoLine"

// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=workgroup_memory_barrier::workgroup_memory_barrier

use spirv_std::spirv;

unsafe fn workgroup_memory_barrier() {
    spirv_std::arch::workgroup_memory_barrier();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        workgroup_memory_barrier();
    }
}
