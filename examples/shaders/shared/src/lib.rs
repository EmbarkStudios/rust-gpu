//! Ported to Rust from https://github.com/Tw1ddle/Sky-Shader/blob/master/src/shaders/glsl/sky.fragment

#![cfg_attr(target_arch = "spirv", no_std)]
#![feature(lang_items)]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::f32::consts::PI;
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
