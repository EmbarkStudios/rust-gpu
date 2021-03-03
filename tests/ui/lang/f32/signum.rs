// Test that `signum` works.
// build-pass

use spirv_std::storage_class::{Input, Output};
use spirv_std::num_traits::Float;

#[spirv(fragment)]
pub fn main(i: Input<f32>, mut o: Output<f32>) {
    *o = (*i).signum();
}
