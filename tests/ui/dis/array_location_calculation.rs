// build-pass
// compile-flags: -C llvm-args=--disassemble-globals

use spirv_std as _;

#[spirv(fragment)]
pub fn main(one: [f32; 7], two: [f32; 3], three: f32) {}
