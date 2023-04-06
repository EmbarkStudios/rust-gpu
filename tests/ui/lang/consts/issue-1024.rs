// Tests that the zombie `bool` from `overflowing_*` (the "has overflowed" value)
// isn't kept alive by the user's own (unrelated) `bool` constants.
//
// NOTE(eddyb) quick explanation for the compile-flags below:
// * `-Coverflow-checks=off`: the default, but explicit here because of how
//   its behavior is relied upon by `u32_overflowing_add_only_result`
// * `-Ccodegen-units=1`: avoids `bool` consts and `<u32 as Add>::add` being
//   in separate CGUs (which then bypasses const deduplication, during linking)

// build-pass
// compile-flags: -Coverflow-checks=off -Ccodegen-units=1

use spirv_std::spirv;

const PRIVATE_GLOBAL_BOOLS: &[bool; 2] = &[false, true];

/// `a.overflowing_add(b).0` equivalent, but with the Rust `(u32, bool`) never
/// existing, and the `.0` happening in the compiler, via `CheckedBinaryOp(Add)`
/// in `<u32 as Add>::add`, and `-Coverflow-checks=off` ignoring the `.1` `bool`.
fn u32_overflowing_add_only_result(a: u32, b: u32) -> u32 {
    <u32 as core::ops::Add>::add(a, b)
}

#[spirv(fragment)]
pub fn main(x: &mut u32) {
    *x = 0;
    for i in 0..PRIVATE_GLOBAL_BOOLS.len() {
        let y = (PRIVATE_GLOBAL_BOOLS[i] as u32) << i;
        *x = u32_overflowing_add_only_result(*x, y);
    }
}
