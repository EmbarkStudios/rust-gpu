// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=device_memory_barrier::device_memory_barrier

use spirv_std as _;

unsafe fn device_memory_barrier() {
    spirv_std::arch::device_memory_barrier();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        device_memory_barrier();
    }
}
