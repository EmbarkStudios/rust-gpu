// Test `&'static &'static T` constants where the `T` values don't themselves
// contain references, and where the `T` values aren't immediatelly loaded from.

// build-pass
// compile-flags: -C target-feature=+VariablePointers

use spirv_std as _;

use glam::{const_mat2, Mat2, Vec2};

#[inline(never)]
fn deep_load(r: &'static &'static u32) -> u32 {
    **r
}

const ROT90: &Mat2 = &const_mat2![[0.0, 1.0], [-1.0, 0.0]];

#[inline(never)]
fn deep_transpose(r: &'static &'static Mat2) -> Mat2 {
    r.transpose()
}

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(flat)] scalar_out: &mut u32,
    #[rust_gpu::spirv(push_constant)] vec_in: &Vec2,
    #[rust_gpu::spirv(flat)] bool_out: &mut u32,
    vec_out: &mut Vec2,
) {
    *scalar_out = deep_load(&&123);
    *bool_out = (vec_in == &Vec2::ZERO) as u32;
    *vec_out = deep_transpose(&ROT90) * *vec_in;
}
