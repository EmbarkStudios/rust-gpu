// Test that using `(a..b).contains(&x)`, which is starting to get used
// in `core` (see https://github.com/rust-lang/rust/pull/87723), cannot
// cause a fatal error, but at most a zombie or SPIR-V validation error.

// build-fail

// HACK(eddyb) this allows CI (older?) `spirv-val` output to also work.
// normalize-stderr-test "  %\d+ = OpVariable %\w+ Function\n\n" -> ""

use spirv_std as _;

fn has_two_decimal_digits(x: u32) -> bool {
    (10..100).contains(&x)
}

#[spirv(fragment)]
pub fn main(i: u32, o: &mut bool) {
    *o = has_two_decimal_digits(i);
}
