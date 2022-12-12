// normalize-stderr-not_spirt "OpLine %5 16 1" -> "OpNoLine"

// Test that non-immediate (i.e. not one of scalar/scalar-pair/vector) inputs
// get properly copied out of the global (`Input`) `OpVariable` and mutation is
// only ever done on `fn`-local `OpVariable`s, not on the original global.

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(mut in_array: [f32; 3], out_array: &mut [f32; 3]) {
    in_array[0] += 1.0;
    *out_array = in_array;
}
