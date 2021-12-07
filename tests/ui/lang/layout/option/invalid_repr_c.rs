// Test that repr(C) will error if the underlying type is not representable

// build-fail

use spirv_std as _;

#[repr(C)]
pub enum Unrepresentable {
    This(u32),
    That(u64),
}

#[spirv(fragment)]
pub fn main(#[spirv(flat)] _input: Unrepresentable) {}
