#![cfg_attr(
    target_arch = "spirv",
    no_std,
    feature(register_attr),
    register_attr(spirv)
)]

#[cfg(not(target_arch = "spirv"))]
#[macro_use]
pub extern crate spirv_std_macros;
use spirv_std::glam::{vec4, Vec4};
use spirv_std::storage_class::{Input, Output};

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main_fs(mut output: Output<Vec4>) {
    output.store(vec4(1.0, 0.0, 0.0, 1.0))
}

#[allow(unused_attributes)]
#[spirv(vertex)]
pub fn main_vs(
    #[spirv(vertex_index)] vert_id: Input<i32>,
    #[spirv(position)] mut out_pos: Output<Vec4>,
) {
    let vert_id = vert_id.load();
    out_pos.store(vec4(
        (vert_id - 1) as f32,
        ((vert_id & 1) * 2 - 1) as f32,
        0.0,
        1.0,
    ));
}
