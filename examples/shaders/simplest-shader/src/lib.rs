#![cfg_attr(
    target_arch = "spirv",
    no_std,
    feature(register_attr),
    register_attr(spirv)
)]

#[cfg(not(target_arch = "spirv"))]
#[macro_use]
pub extern crate spirv_std_macros;
use spirv_std::glam::{vec4, Vec2, Vec4};
use spirv_std::storage_class::{Input, Output};

#[allow(unused_attributes)]
#[spirv(vertex)]
pub fn main_vs(mut uv: Output<Vec4>, #[spirv(position)] mut builtin_pos: Output<Vec4>) {
    uv.store(Vec4::new(0.0, 0.0, 0.0, 0.0));
    builtin_pos.store(Vec4::new(0.0, 0.0, 0.0, 0.0));
}

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main_fs(uv: Input<Vec2>, mut out_color: Output<Vec4>) {
    out_color.store(Vec4::new(uv.load().x, 0.0, 0.0, 0.0));
}
