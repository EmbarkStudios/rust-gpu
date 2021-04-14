// Test `&'static T` constants where the `T` values don't themselves contain
// references, and where the `T` values aren't immediatelly loaded from.

// build-pass

use spirv_std as _;

use glam::{const_mat2, Mat2, Vec2};

#[inline(never)]
fn scalar_load(r: &'static u32) -> u32 {
    *r
}

const ROT90: Mat2 = const_mat2![[0.0, 1.0], [-1.0, 0.0]];

#[spirv(fragment)]
pub fn main(scalar_out: &mut u32, vec_in: Vec2, bool_out: &mut bool, vec_out: &mut Vec2) {
    *scalar_out = scalar_load(&123);
    *bool_out = vec_in == Vec2::ZERO;
    *vec_out = ROT90.transpose() * vec_in;
}
