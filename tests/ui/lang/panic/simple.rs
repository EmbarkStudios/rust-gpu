// Test that calling `panic!` works.
// build-pass

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main() {
    panic!("aaa");
}
