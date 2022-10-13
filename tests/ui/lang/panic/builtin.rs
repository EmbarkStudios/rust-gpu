// Test panics coming from the Rust language such as `1 / 0`.
// build-pass

use spirv_std as _;

fn int_div(x: usize) -> usize {
    1 / x
}

#[rust_gpu::spirv(fragment)]
pub fn main() {
    int_div(0);
}
