// build-fail

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn fragment(int: u32, double: f64) {}
