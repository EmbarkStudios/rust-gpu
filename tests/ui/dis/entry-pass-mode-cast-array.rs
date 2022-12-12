// normalize-stderr-not_spirt "OpLine %5 18 1" -> "OpNoLine"

// This is a similar setup to the `issue-731` test, but instead of "just" the
// missing copy out of the global (`Input`) `OpVariable`, small enough types
// would fail much earlier (by generating unsupported pointer casts).
// (Just like `issue-373`, the problem was the use of `PassMode::Cast`, through
// the default Rust ABI adjustments, that we now override through query hooks)

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(mut in_array: [f32; 2], out_array: &mut [f32; 2]) {
    in_array[0] += 1.0;
    *out_array = in_array;
}
