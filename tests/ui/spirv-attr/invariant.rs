// Tests that the invariant attribute works
// build-pass

use spirv_std as _;

#[rust_gpu::spirv(vertex)]
pub fn main(#[rust_gpu::spirv(invariant)] output: &mut f32) {}
