// build-fail

use spirv_std as _;

#[spirv(fragment)]
pub fn fragment(int: u32, double: f64) {}
