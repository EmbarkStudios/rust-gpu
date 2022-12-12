// normalize-stderr-not_spirt "OpLine %7 10 1" -> "OpNoLine"

// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=add_two_ints::add_two_ints

use spirv_std::spirv;

fn add_two_ints(x: u32, y: u32) -> u32 {
    x + y
}
#[spirv(fragment)]
pub fn main() {
    add_two_ints(2, 3);
}
