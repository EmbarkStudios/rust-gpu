// build-pass
// compile-flags: -C llvm-args=--disassemble-globals

use spirv_std::{self as _, glam::{Mat3, DVec3, IVec4}};

#[spirv(fragment)]
pub fn main(one: [f32; 7], two: [f32; 3], three: Mat3, four: DVec3, five: IVec4, six: f32, seven: u32) {}
