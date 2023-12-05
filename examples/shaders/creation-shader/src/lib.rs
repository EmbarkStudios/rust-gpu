// Ported from https://www.shadertoy.com/view/XsXXDn.
// Credits to Danilo Guanabara.

#![cfg_attr(target_arch = "spirv", no_std)]
// HACK(eddyb) can't easily see warnings otherwise from `spirv-builder` builds.
#![deny(warnings)]

use glam::{vec2, vec4, Vec2, Vec3, Vec4, Vec4Swizzles};
use shared::*;
#[allow(warnings)]
use spirv_std::num_traits::Float as _;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main_fs(
    #[spirv(frag_coord)] in_frag_coord: Vec4,
    #[spirv(push_constant)] constants: &ShaderConstants,
    output: &mut Vec4,
) {
    let t = constants.time;
    let resolution = vec2(constants.width as f32, constants.height as f32);

    let mut l = t;
    let mut z = t;
    let mut c = Vec3::ZERO;

    for i in 0..3 {
        let mut uv;
        let mut p = in_frag_coord.xy() / resolution;
        uv = p;
        p -= 0.5;
        p.x *= resolution.x / resolution.y;
        z += 0.07;
        l = p.length();
        uv += p / l * (z.sin() + 1.0) * (l * 9.0 - z - z).sin().abs();

        let tmp = (uv % 1.0) - 0.5;
        let val = 0.01 / tmp.length();
        match i {
            0 => c.x = val,
            1 => c.y = val,
            2 => c.z = val,
            _ => unreachable!(),
        };
    }

    let tmp = c / l;

    *output = vec4(tmp.x, tmp.y, tmp.z, t);
}

#[spirv(vertex)]
pub fn main_vs(#[spirv(vertex_index)] vert_idx: i32, #[spirv(position)] builtin_pos: &mut Vec4) {
    // Create a "full screen triangle" by mapping the vertex index.
    // ported from https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
    let uv = vec2(((vert_idx << 1) & 2) as f32, (vert_idx & 2) as f32);
    let pos = 2.0 * uv - Vec2::ONE;

    *builtin_pos = pos.extend(0.0).extend(1.0);
}
