// Ported from https://www.shadertoy.com/view/DlGfzh

#![cfg_attr(target_arch = "spirv", no_std)]
// HACK(eddyb) can't easily see warnings otherwise from `spirv-builder` builds.
#![deny(warnings)]

use glam::{vec2, Vec2, Vec3, Vec4};
use shared::*;
#[allow(warnings)]
use spirv_std::num_traits::Float as _;
use spirv_std::{num_traits::Float, spirv};

#[derive(Clone, Copy)]
struct Time(f32);

fn h(a: f32) -> Vec3 {
    let mut tmp =
        Vec3::new(-10.0.to_radians(), 60.0.to_radians(), 120.0.to_radians()) + (a) * 6.2832;
    tmp.x = tmp.x.cos();
    tmp.y = tmp.y.cos();
    tmp.z = tmp.z.cos();
    tmp * 0.5 + 0.5
}

#[spirv(fragment)]
pub fn main_fs(
    #[spirv(frag_coord)] in_frag_coord: Vec4,
    #[spirv(push_constant)] constants: &ShaderConstants,
    output: &mut Vec4,
) {
    let r = vec2(constants.width as f32, constants.height as f32);
    let o = (in_frag_coord + in_frag_coord - r.extend(1.0).extend(1.0)) / r.y;
    let m = (vec2(constants.cursor_x.round(), constants.cursor_y.round()) - r / 2.0) / r.y * 5.0
        + constants.time / 10.0;
    let mut u;
    let mut g;
    let mut c = Vec3::ZERO;
    for i in 0..100 {
        let i = i as f32 * 0.01;
        u = o * i;
        let z = (1.0 - u.dot(u)).max(0.0);
        u /= 0.1 + z.sqrt() * 0.3;
        u.x = u.x / 0.866 - m.x;
        u.y += (u.x.ceil() * 0.5).fract() - m.y;
        g = (u.fract() - 0.5).abs();
        c += h(i) / 250.0 * z / (((g.x * 1.5 + g).max(g + g).y - 1.0).abs() + 0.1 - i * 0.08);
    }

    *output = to_linear(Vec4::new(
        (c.x * c.x).tanh(),
        (c.y * c.y).tanh(),
        (c.z * c.z).tanh(),
        1.0,
    ));
}

#[spirv(vertex)]
pub fn main_vs(#[spirv(vertex_index)] vert_idx: i32, #[spirv(position)] builtin_pos: &mut Vec4) {
    // Create a "full screen triangle" by mapping the vertex index.
    // ported from https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
    let uv = vec2(((vert_idx << 1) & 2) as f32, (vert_idx & 2) as f32);
    let pos = 2.0 * uv - Vec2::ONE;

    *builtin_pos = pos.extend(0.0).extend(1.0);
}
