//! Ported to Rust from https://github.com/Tw1ddle/Sky-Shader/blob/master/src/shaders/glsl/sky.fragment

#![cfg_attr(target_arch = "spirv", no_std)]
#![feature(lang_items)]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::f32::consts::{FRAC_1_PI, PI};
use spirv_std::glam::{Vec2, Vec3};
use spirv_std::MathExt;

#[derive(Copy, Clone)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

// TODO: add this to glam? Rust std has it on f32/f64
pub fn pow(v: Vec3, power: f32) -> Vec3 {
    Vec3::new(v.x().pow(power), v.y().pow(power), v.z().pow(power))
}

// TODO: add this to glam? Rust std has it on f32/f64
pub fn exp(v: Vec3) -> Vec3 {
    Vec3::new(v.x().exp(), v.y().exp(), v.z().exp())
}

/// Based on: https://seblagarde.wordpress.com/2014/12/01/inverse-trigonometric-functions-gpu-optimization-for-amd-gcn-architecture/
pub fn acos_approx(v: f32) -> f32 {
    let x = v.abs();
    let mut res = -0.155972 * x + 1.56467; // p(x)
    res *= (1.0f32 - x).sqrt();

    if v >= 0.0 {
        res
    } else {
        PI - res
    }
}

pub fn hash(p: Vec3) -> f32 {
    let p = p * Vec3::splat(FRAC_1_PI) + Vec3::splat(0.1);
    let mut p = Vec3::new(p.x().fract(), p.y().fract(), p.z().fract());
    p *= 17.0;
    (p.x() * p.y() * p.z() * (p.x() + p.y() + p.z())).fract()
}

pub fn mix(x: f32, y: f32, s: f32) -> f32 {
    x + s * (y - x)
}

pub fn noise(x: Vec3) -> f32 {
    let i = x.floor();
    let mut f = Vec3::new(x.x().fract(), x.y().fract(), x.z().fract());
    f = f * f * (Vec3::splat(3.0) - Vec3::splat(2.0) * f);

    -1.0 + 2.0
        * mix(
            mix(
                mix(
                    hash(i + Vec3::new(0.0, 0.0, 0.0)),
                    hash(i + Vec3::new(1.0, 0.0, 0.0)),
                    f.x(),
                ),
                mix(
                    hash(i + Vec3::new(0.0, 1.0, 0.0)),
                    hash(i + Vec3::new(1.0, 1.0, 0.0)),
                    f.x(),
                ),
                f.y(),
            ),
            mix(
                mix(
                    hash(i + Vec3::new(0.0, 0.0, 1.0)),
                    hash(i + Vec3::new(1.0, 0.0, 1.0)),
                    f.x(),
                ),
                mix(
                    hash(i + Vec3::new(0.0, 1.0, 1.0)),
                    hash(i + Vec3::new(1.0, 1.0, 1.0)),
                    f.x(),
                ),
                f.y(),
            ),
            f.z(),
        )
}

/// renamed because of cross-compilation issues with spirv-cross/ moltenvk
pub fn my_smoothstep(edge0: f32, edge1: f32, x: f32) -> f32 {
    // Scale, bias and saturate x to 0..1 range
    let x = ((x - edge0) / (edge1 - edge0)).saturate();
    // Evaluate polynomial
    x * x * (3.0 - 2.0 * x)
}

pub fn tonemap(col: Vec3) -> Vec3 {
    // see https://www.desmos.com/calculator/0eo9pzo1at
    const A: f32 = 2.35;
    const B: f32 = 2.8826666;
    const C: f32 = 789.7459;
    const D: f32 = 0.935;

    let z = pow(col, A);
    z / (pow(z, D) * B + Vec3::splat(C))
}

pub fn get_ray_dir(uv: Vec2, pos: Vec3, look_at_pos: Vec3) -> Vec3 {
    let forward = (look_at_pos - pos).normalize();
    let right = Vec3::new(0.0, 1.0, 0.0).cross(forward).normalize();
    let up = forward.cross(right);
    (forward + uv.x() * right + uv.y() * up).normalize()
}
