#![crate_name = "issue_1062"]

// Test that rotates take the correct path for non-zero bit amounts.

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] x: u32, #[spirv(flat)] s: u32, out: &mut (u32, u32)) {
    *out = (x.rotate_left(s), x.rotate_right(s));
}
