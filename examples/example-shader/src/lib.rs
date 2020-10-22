//! Ported to Rust from https://github.com/Tw1ddle/Sky-Shader/blob/master/src/shaders/glsl/sky.fragment

#![no_std]
#![feature(lang_items)]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::f32::consts::PI;
#[cfg(not(test))]
use core::panic::PanicInfo;
use spirv_std::{Input, Mat4, MathExt, Output, Vec3, Vec4};

const DEPOLARIZATION_FACTOR: f32 = 0.035;
const LUMINANCE: f32 = 1.0;
const MIE_COEFFICIENT: f32 = 0.005;
const MIE_DIRECTIONAL_G: f32 = 0.8;
const MIE_K_COEFFICIENT: Vec3 = Vec3::new(0.686, 0.678, 0.666);
const MIE_V: f32 = 4.0;
const MIE_ZENITH_LENGTH: f32 = 1.25e3;
const NUM_MOLECULES: f32 = 2.542e25f32;
const PRIMARIES: Vec3 = Vec3::new(6.8e-7f32, 5.5e-7f32, 4.5e-7f32);
const RAYLEIGH: f32 = 1.0;
const RAYLEIGH_ZENITH_LENGTH: f32 = 8.4e3;
const REFRACTIVE_INDEX: f32 = 1.0003;
const SUN_ANGULAR_DIAMETER_DEGREES: f32 = 0.0093333;
const SUN_INTENSITY_FACTOR: f32 = 1000.0;
const SUN_INTENSITY_FALLOFF_STEEPNESS: f32 = 1.5;
const TONEMAP_WEIGHTING: Vec3 = Vec3::splat(9.50);
const TURBIDITY: f32 = 2.0;

/// Based on: https://seblagarde.wordpress.com/2014/12/01/inverse-trigonometric-functions-gpu-optimization-for-amd-gcn-architecture/
fn acos_approx(v: f32) -> f32 {
    let x = v.abs();
    let mut res = -0.155972 * x + 1.56467; // p(x)
    res *= (1.0f32 - x).sqrt();

    let mask = (v >= 0.0) as u32 as f32;

    // can't use if-statement so do oldskool shader masking instead to avoid conditional
    (res * mask) + ((1.0f32 - mask) * (PI - res))
}

/// renamed because of cross-compilation issues with spirv-cross/ moltenvk
fn my_smoothstep(edge0: f32, edge1: f32, x: f32) -> f32 {
    // Scale, bias and saturate x to 0..1 range
    let x = ((x - edge0) / (edge1 - edge0)).saturate();
    // Evaluate polynomial
    x * x * (3.0 - 2.0 * x)
}

fn total_rayleigh(lambda: Vec3) -> Vec3 {
    (8.0 * PI.pow(3.0)
        * (REFRACTIVE_INDEX.pow(2.0) - 1.0).pow(2.0)
        * (6.0 + 3.0 * DEPOLARIZATION_FACTOR))
        / (3.0 * NUM_MOLECULES * lambda.pow(4.0) * (6.0 - 7.0 * DEPOLARIZATION_FACTOR))
}

fn total_mie(lambda: Vec3, k: Vec3, t: f32) -> Vec3 {
    let c = 0.2 * t * 10e-18;
    0.434 * c * PI * ((2.0 * PI) / lambda).pow(MIE_V - 2.0) * k
}

fn rayleigh_phase(cos_theta: f32) -> f32 {
    (3.0 / (16.0 * PI)) * (1.0 + cos_theta.pow(2.0))
}

fn henyey_greenstein_phase(cos_theta: f32, g: f32) -> f32 {
    (1.0 / (4.0 * PI)) * ((1.0 - g.pow(2.0)) / (1.0 - 2.0 * g * cos_theta + g.pow(2.0)).pow(1.5))
}

fn sun_intensity(zenith_angle_cos: f32) -> f32 {
    let cutoff_angle = PI / 1.95; // Earth shadow hack
    SUN_INTENSITY_FACTOR
        * 0.0f32.max(
            1.0 - (-((cutoff_angle - acos_approx(zenith_angle_cos))
                / SUN_INTENSITY_FALLOFF_STEEPNESS))
                .exp(),
        )
}

fn uncharted2_tonemap(w: Vec3) -> Vec3 {
    const A: Vec3 = Vec3::splat(0.15); // Shoulder strength
    const B: Vec3 = Vec3::splat(0.50); // Linear strength
    const C: Vec3 = Vec3::splat(0.10); // Linear angle
    const D: Vec3 = Vec3::splat(0.20); // Toe strength
    const E: Vec3 = Vec3::splat(0.02); // Toe numerator
    const F: Vec3 = Vec3::splat(0.30); // Toe denominator

    ((w * (A * w + C * B) + D * E) / (w * (A * w + B) + D * F)) - E / F
}

fn sky(dir: Vec3, sun_position: Vec3) -> Vec3 {
    let up = Vec3::new(0.0, 1.0, 0.0);
    let sunfade = 1.0 - (1.0 - (sun_position.1 / 450000.0).exp()).saturate();
    let rayleigh_coefficient = RAYLEIGH - (1.0 * (1.0 - sunfade));
    let beta_r = total_rayleigh(PRIMARIES) * rayleigh_coefficient;

    // Mie coefficient
    let beta_m = total_mie(PRIMARIES, MIE_K_COEFFICIENT, TURBIDITY) * MIE_COEFFICIENT;

    // Optical length, cutoff angle at 90 to avoid singularity
    let zenith_angle = acos_approx(up.dot(dir).max(0.0));
    let denom = (zenith_angle).cos() + 0.15 * (93.885 - ((zenith_angle * 180.0) / PI)).pow(-1.253);

    let s_r = RAYLEIGH_ZENITH_LENGTH / denom;
    let s_m = MIE_ZENITH_LENGTH / denom;

    // Combined extinction factor
    let fex = (-(beta_r * s_r + beta_m * s_m)).exp();

    // In-scattering
    let sun_direction = sun_position.normalize();
    let cos_theta = dir.dot(sun_direction);
    let beta_r_theta = beta_r * rayleigh_phase(cos_theta * 0.5 + 0.5);

    let beta_m_theta = beta_m * henyey_greenstein_phase(cos_theta, MIE_DIRECTIONAL_G);
    let sun_e = sun_intensity(sun_direction.dot(up));
    let mut lin =
        (sun_e * ((beta_r_theta + beta_m_theta) / (beta_r + beta_m)) * (Vec3::splat(1.0) - fex))
            .pow(1.5);
    lin *= Vec3::splat(1.0).lerp(
        (sun_e * ((beta_r_theta + beta_m_theta) / (beta_r + beta_m)) * fex).pow(0.5),
        ((1.0 - up.dot(sun_direction)).pow(5.0)).saturate(),
    );

    // Composition + solar disc
    let sun_angular_diameter_cos = SUN_ANGULAR_DIAMETER_DEGREES.cos();
    let sundisk = my_smoothstep(
        sun_angular_diameter_cos,
        sun_angular_diameter_cos + 0.00002,
        cos_theta,
    );
    let mut l0 = 0.1 * fex;
    l0 += sun_e * 19000.0 * fex * sundisk;
    let mut tex_color = lin + l0;
    tex_color *= Vec3::splat(0.04);
    tex_color += Vec3::new(0.0, 0.001, 0.0025) * 0.3;

    // Tonemapping
    let white_scale = 1.0 / uncharted2_tonemap(TONEMAP_WEIGHTING);
    let curr = uncharted2_tonemap(((2.0 / LUMINANCE.pow(4.0)).log2()) * tex_color);
    let color = curr * white_scale;

    color.pow(1.0 / (1.2 + (1.2 * sunfade)))
}

#[allow(unused_attributes)]
#[spirv(entry = "fragment")]
pub fn main_fs(input: Input<Vec4>, mut output: Output<Vec4>) {
    let dir: Vec3 = input.load().truncate();

    // hard-code information because we can't bind buffers at the moment
    let eye_pos = Vec3(0.0, 0.0997, 0.2);
    let sun_pos = Vec3::new(0.0, 75.0, -1000.0);
    let clip_to_world = Mat4 {
        x_axis: Vec4(-0.5522849, 0.0, 0.0, 0.0),
        y_axis: Vec4(0.0, 0.4096309, -0.061444636, 0.0),
        z_axis: Vec4(0.0, 99.99999, 199.99998, 999.99994),
        w_axis: Vec4(0.0, -0.14834046, -0.98893654, 0.0),
    };

    let cs_pos = Vec4(dir.0, -dir.1, 1.0, 1.0);
    let ws_pos = {
        let p = clip_to_world.mul_vec4(cs_pos);
        p.truncate() / p.3
    };
    let dir = (ws_pos - eye_pos).normalize();

    // evaluate Preetham sky model
    let color = sky(dir, sun_pos);

    output.store(color.extend(0.0))
}

#[allow(unused_attributes)]
#[spirv(entry = "vertex")]
pub fn main_vs(
    in_pos: Input<Vec4>,
    _in_color: Input<Vec4>,
    #[spirv(builtin = "position")] mut out_pos: Output<Vec4>,
    mut out_color: Output<Vec4>,
) {
    out_pos.store(in_pos.load());
    out_color.store(in_pos.load());
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

#[cfg(not(test))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}
