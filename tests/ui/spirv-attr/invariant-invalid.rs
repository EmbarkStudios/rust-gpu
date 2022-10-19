// Tests that the invariant attribute can't be applied on inputs
// build-fail

use spirv_std::spirv;

#[spirv(vertex)]
pub fn main(#[spirv(invariant)] input: f32) {}
