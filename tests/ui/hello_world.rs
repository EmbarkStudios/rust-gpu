// Simple single entrypoint function test.
// build-pass

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main() {}
