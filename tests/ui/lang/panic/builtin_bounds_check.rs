// Test that bounds checking causes panics.
// build-pass

use spirv_std as _;

fn array_bounds_check(x: [u32; 4], i: usize) -> u32 {
    x[i]
}

#[spirv(fragment)]
pub fn main() {
    array_bounds_check([0, 1, 2, 3], 5);
}
