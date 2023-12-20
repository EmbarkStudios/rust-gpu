// Ported from https://www.shadertoy.com/view/tsXBzS.

#![cfg_attr(target_arch = "spirv", no_std)]
// HACK(eddyb) can't easily see warnings otherwise from `spirv-builder` builds.
#![deny(warnings)]

use glam::{vec2, Vec2, Vec3, Vec4};
use shared::*;
#[allow(warnings)]
use spirv_std::num_traits::Float as _;
use spirv_std::{
    glam::{mat2, Vec2Swizzles, Vec3Swizzles, Vec4Swizzles},
    spirv,
};

#[derive(Clone, Copy)]
struct Time(f32);

fn palette(d: f32) -> Vec3 {
    Vec3::new(0.2, 0.7, 0.9).lerp(Vec3::new(1.0, 0.0, 1.0), d)
}

fn rotate(p: Vec2, a: f32) -> Vec2 {
    let c = a.cos();
    let s = a.sin();
    mat2(Vec2::new(c, s), Vec2::new(-s, c)).mul_vec2(p)
}

fn map(mut p: Vec3, time: Time) -> f32 {
    let t = time.0 * 0.2;
    for _ in 0..8 {
        // p.xz =rotate(p.xz,t);
        let tmp = rotate(p.xz(), t);
        p.x = tmp.x;
        p.z = tmp.y;
        // p.xy =rotate(p.xy,t*1.89);
        let tmp = rotate(p.xy(), t * 1.89);
        p.x = tmp.x;
        p.y = tmp.y;
        // p.xz = abs(p.xz);
        let tmp = p.xz().abs();
        p.x = tmp.x;
        p.z = tmp.y;
        // p.xz-=.5;
        p.x -= 0.5;
        p.z -= 0.5;
    }

    p.signum().dot(p) / 5.0
}

fn rm(ro: Vec3, rd: Vec3, time: Time) -> Vec4 {
    let mut t = 0.0;
    let mut col = Vec3::ZERO;
    let mut d = 0.0;
    for _ in 0..64 {
        let p = ro + rd * t;
        d = map(p, time) * 0.5;
        if d < 0.02 {
            break;
        }
        if d > 100.0 {
            break;
        }
        col += palette(p.length() * 0.1) / (400.0 * (d));
        t += d;
    }
    Vec4::new(col.x, col.y, col.z, 1.0 / (d * 100.0))
}

#[spirv(fragment)]
pub fn main_fs(
    #[spirv(frag_coord)] in_frag_coord: Vec4,
    #[spirv(push_constant)] constants: &ShaderConstants,
    output: &mut Vec4,
) {
    let resolution = vec2(constants.width as f32, constants.height as f32);

    let uv = (in_frag_coord.xy() - (resolution.xy() / 2.0)) / resolution;
    let mut ro = Vec3::new(0.0, 0.0, -50.0);
    // ro.xz = rotate(ro.xz,iTime);
    let tmp = rotate(ro.xz(), constants.time);
    ro.x = tmp.x;
    ro.z = tmp.y;
    let cf = (-ro).normalize();
    let cs = cf.cross(Vec3::new(0.0, 1.0, 0.0)).normalize();
    let cu = cf.cross(cs).normalize();

    let uuv = ro + cf * 3.0 + uv.x * cs + uv.y * cu;
    let rd = (uuv - ro).normalize();
    let col = rm(ro, rd, Time(constants.time)).clamp(Vec4::ZERO, Vec4::ONE);
    *output = to_linear(Vec4::new(col.x, col.y, col.z, col.w));
}

#[spirv(vertex)]
pub fn main_vs(#[spirv(vertex_index)] vert_idx: i32, #[spirv(position)] builtin_pos: &mut Vec4) {
    // Create a "full screen triangle" by mapping the vertex index.
    // ported from https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
    let uv = vec2(((vert_idx << 1) & 2) as f32, (vert_idx & 2) as f32);
    let pos = 2.0 * uv - Vec2::ONE;

    *builtin_pos = pos.extend(0.0).extend(1.0);
}
