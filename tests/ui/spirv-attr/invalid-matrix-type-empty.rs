// Tests that matrix type inference fails correctly, for empty struct
// build-fail

use spirv_std::spirv;

#[spirv(matrix)]
pub struct EmptyStruct {}

#[spirv(fragment)]
pub fn entry(#[spirv(push_constant)] matrix: &EmptyStruct) {}
