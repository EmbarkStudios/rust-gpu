// Tests that the invariant attribute works
// build-pass

use spirv_std::spirv;

#[spirv(vertex)]
pub fn main(#[spirv(invariant)] output: &mut f32) {}
