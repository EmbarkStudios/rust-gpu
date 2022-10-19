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

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(flat)] scalar_out: &mut u32,
    vec_in: Vec2,
    #[rust_gpu::spirv(flat)] bool_out: &mut u32,
    vec_out: &mut Vec2,
) {
    *scalar_out = scalar_load(&123);
    *bool_out = (vec_in == Vec2::ZERO) as u32;
    *vec_out = ROT90.transpose() * vec_in;
}
