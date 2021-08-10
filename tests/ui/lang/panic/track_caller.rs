// Test that propagating `#[track_caller]` doesn't cause constant-related errors.

// build-pass

use spirv_std as _;

#[track_caller]
fn track_caller_maybe_panic(x: u32) {
    if x > 0 {
        panic!();
    }
}

#[spirv(fragment)]
pub fn main(x: u32) {
    track_caller_maybe_panic(x);
}
