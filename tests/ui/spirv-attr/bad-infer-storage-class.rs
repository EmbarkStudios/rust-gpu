// Tests that storage class inference fails correctly
// build-fail

use spirv_std::Image2d;

#[spirv(vertex)]
pub fn main(#[spirv(uniform)] error: &Image2d, #[spirv(uniform_constant)] warning: &Image2d) {}
