// build-fail

// FIXME(eddyb) this should work, but `unchecked_add_unsigned` doesn't exist,
// so range internals use `a.checked_add_unsigned(b).unwrap_unchecked()` instead,
// and that requires working `overflowing_add`, which we don't yet have.

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: i32) {
    for _ in 0..i {}
}
