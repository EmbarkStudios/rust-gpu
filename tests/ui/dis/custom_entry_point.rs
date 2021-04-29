// build-pass
// compile-flags: -C llvm-args=--disassemble-globals

use spirv_std as _;

#[spirv(fragment(entry_point_name = "hello_world"))]
pub fn main() {}
