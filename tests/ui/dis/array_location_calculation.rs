// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=add_two_ints::add_two_ints

use spirv_std as _;

#[spirv(fragment)]
pub fn array_locations(one: [f32; 7], two: [f32; 3], three: f32) {}
