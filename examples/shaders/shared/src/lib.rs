//! Ported to Rust from <https://github.com/Tw1ddle/Sky-Shader/blob/master/src/shaders/glsl/sky.fragment>

#![cfg_attr(target_arch = "spirv", no_std, feature(lang_items))]

use core::f32::consts::PI;
use glam::{vec3, Vec3};

pub use spirv_std::glam;

// Note: This cfg is incorrect on its surface, it really should be "are we compiling with std", but
// we tie #[no_std] above to the same condition, so it's fine.
#[cfg(target_arch = "spirv")]
use spirv_std::num_traits::Float;

use bytemuck::{Pod, Zeroable};

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,

    pub cursor_x: f32,
    pub cursor_y: f32,
    pub drag_start_x: f32,
    pub drag_start_y: f32,
    pub drag_end_x: f32,
    pub drag_end_y: f32,

    /// Bit mask of the pressed buttons (0 = Left, 1 = Middle, 2 = Right).
    pub mouse_button_pressed: u32,

    /// The last time each mouse button (Left, Middle or Right) was pressed,
    /// or `f32::NEG_INFINITY` for buttons which haven't been pressed yet.
    ///
    /// If this is the first frame after the press of some button, that button's
    /// entry in `mouse_button_press_time` will exactly equal `time`.
    pub mouse_button_press_time: [f32; 3],
}

pub fn saturate(x: f32) -> f32 {
    x.max(0.0).min(1.0)
}

pub fn pow(v: Vec3, power: f32) -> Vec3 {
    vec3(v.x.powf(power), v.y.powf(power), v.z.powf(power))
}

pub fn exp(v: Vec3) -> Vec3 {
    vec3(v.x.exp(), v.y.exp(), v.z.exp())
}

/// Based on: <https://seblagarde.wordpress.com/2014/12/01/inverse-trigonometric-functions-gpu-optimization-for-amd-gcn-architecture/>
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

pub fn smoothstep(edge0: f32, edge1: f32, x: f32) -> f32 {
    // Scale, bias and saturate x to 0..1 range
    let x = saturate((x - edge0) / (edge1 - edge0));
    // Evaluate polynomial
    x * x * (3.0 - 2.0 * x)
}
