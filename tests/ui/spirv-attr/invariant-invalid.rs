// Tests that the invariant attribute can't be applied on inputs
// build-fail

use spirv_std as _;

#[rust_gpu::spirv(vertex)]
pub fn main(#[rust_gpu::spirv(invariant)] input: f32) {}
