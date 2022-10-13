// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main(#[rust_gpu::spirv(storage_buffer, descriptor_set = 0, binding = 0)] slice: &mut [f32]) {
    let float: f32 = slice[0];
    let _ = float;
}
