// Test that calling `panic!` works.
// build-pass

use spirv_std as _;

#[spirv(fragment)]
pub fn main() {
    panic!("aaa");
}
