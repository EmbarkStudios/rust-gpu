// Tests multiplying a `Mat3` by a `Vec3`.
// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(input: glam::Mat3, output: &mut glam::Vec3) {
    let vector = input * glam::Vec3::new(1.0, 2.0, 3.0);
    *output = vector;
}
