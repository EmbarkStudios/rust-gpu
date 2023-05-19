#![crate_name = "unwrap_or"]

// unwrap_or generates some memory-bools (as u8). Test to make sure they're fused away.
// OpINotEqual, as well as %bool, should not appear in the output.

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(out: &mut u32) {
    *out = None.unwrap_or(15);
}
