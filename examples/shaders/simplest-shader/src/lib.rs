#![cfg_attr(
    target_arch = "spirv",
    no_std,
    feature(register_tool),
    register_tool(rust_gpu)
)]
// HACK(eddyb) can't easily see warnings otherwise from `spirv-builder` builds.
#![deny(warnings)]

use shared::glam::{vec4, Vec4};
#[cfg(not(target_arch = "spirv"))]
use spirv_std::rust_gpu;

#[rust_gpu::spirv(fragment)]
pub fn main_fs(output: &mut Vec4) {
    *output = vec4(1.0, 0.0, 0.0, 1.0);
}

#[rust_gpu::spirv(vertex)]
pub fn main_vs(
    #[rust_gpu::spirv(vertex_index)] vert_id: i32,
    #[rust_gpu::spirv(position, invariant)] out_pos: &mut Vec4,
) {
    *out_pos = vec4(
        (vert_id - 1) as f32,
        ((vert_id & 1) * 2 - 1) as f32,
        0.0,
        1.0,
    );
}
