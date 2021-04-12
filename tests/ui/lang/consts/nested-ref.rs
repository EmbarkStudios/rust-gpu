// Test `&'static &'static T` constants where the `T` values don't themselves
// contain references, and where the `T` values aren't immediatelly loaded from.

// build-pass

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

#[spirv(fragment)]
pub fn main(
    scalar_out: &mut u32,
    #[spirv(push_constant)] vec_in: &Vec2,
    bool_out: &mut bool,
    vec_out: &mut Vec2,
) {
    unsafe { asm!("OpCapability VariablePointers") };
    *scalar_out = deep_load(&&123);
    *bool_out = vec_in == &Vec2::ZERO;
    *vec_out = deep_transpose(&ROT90) * *vec_in;
}
