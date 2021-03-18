// Tests muiltplying a `Mat3` by a `Vec3`.
// build-pass

use spirv_std::storage_class::{Input, Output};

#[spirv(fragment)]
pub fn main(input: Input<glam::Mat3>, mut output: Output<glam::Vec3>) {
    let input = *input;
    let vector = input * glam::Vec3::new(1.0, 2.0, 3.0);
    *output = vector;
}
