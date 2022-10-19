use spirv_std::spirv;

// Test that `signum` works.
// build-pass

use spirv_std::num_traits::Float;

#[spirv(fragment)]
pub fn main(i: f32, o: &mut f32) {
    *o = i.signum();
}
