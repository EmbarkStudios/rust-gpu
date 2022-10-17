// Test that calling `panic!` works.
// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    panic!("aaa");
}
