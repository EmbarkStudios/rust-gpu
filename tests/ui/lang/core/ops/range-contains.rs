// Test that using `(a..b).contains(&x)`, which is starting to get used
// in `core` (see https://github.com/rust-lang/rust/pull/87723), cannot
// cause a fatal error, but at most a zombie or SPIR-V validation error.

// build-pass

use spirv_std as _;

fn has_two_decimal_digits(x: u32) -> bool {
    (10..100).contains(&x)
}

#[rust_gpu::spirv(fragment)]
pub fn main(#[rust_gpu::spirv(flat)] i: u32, #[rust_gpu::spirv(flat)] o: &mut u32) {
    *o = has_two_decimal_digits(i) as u32;
}
