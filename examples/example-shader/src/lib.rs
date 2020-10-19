#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]
#![feature(core_intrinsics)]
#![allow(dead_code)]

use core::ops::Add;
use core::ops::AddAssign;
use core::ops::Div;
use core::ops::Mul;
use core::ops::MulAssign;
use core::ops::Neg;
use core::ops::Sub;
use core::panic::PanicInfo;
use spirv_std::{f32x4, Input, Output};

/// A 3-dimensional vector without SIMD support.
#[derive(Clone, Copy, PartialEq, PartialOrd, Debug, Default)]
#[repr(C)]
pub struct Vec3(pub(crate) f32, pub(crate) f32, pub(crate) f32);

/// Creates a `Vec3`.
#[inline]
pub fn vec3(x: f32, y: f32, z: f32) -> Vec3 {
    Vec3::new(x, y, z)
}

impl Vec3 {
    /// Creates a new `Vec3`.
    #[inline]
    pub const fn new(x: f32, y: f32, z: f32) -> Self {
        Self(x, y, z)
    }

    /// Creates a `Vec3` with all elements set to `0.0`.
    #[inline]
    pub const fn zero() -> Self {
        Self::new(0.0, 0.0, 0.0)
    }

    /// Creates a `Vec3` with all elements set to `1.0`.
    #[inline]
    pub const fn one() -> Self {
        Self::new(1.0, 1.0, 1.0)
    }

    /// Creates a `Vec3` with values `[x: 1.0, y: 0.0, z: 0.0]`.
    #[inline]
    pub const fn unit_x() -> Self {
        Self::new(1.0, 0.0, 0.0)
    }

    /// Creates a `Vec3` with values `[x: 0.0, y: 1.0, z: 0.0]`.
    #[inline]
    pub const fn unit_y() -> Self {
        Self::new(0.0, 1.0, 0.0)
    }

    /// Creates a `Vec3` with values `[x: 0.0, y: 0.0, z: 1.0]`.
    #[inline]
    pub const fn unit_z() -> Self {
        Self::new(0.0, 0.0, 1.0)
    }

    /// Creates a `Vec3` with all elements set to `v`.
    #[inline]
    pub const fn splat(v: f32) -> Self {
        Self(v, v, v)
    }

    pub fn pow(&self, factor: f32) -> Self {
        unsafe {
            Self(
                core::intrinsics::powf32(self.0, factor),
                core::intrinsics::powf32(self.1, factor),
                core::intrinsics::powf32(self.2, factor),
            )
        }
    }
}

impl Mul<f32> for Vec3 {
    type Output = Self;
    #[inline]
    fn mul(self, other: f32) -> Vec3 {
        Self(self.0 * other, self.1 * other, self.2 * other)
    }
}

impl Add<f32> for Vec3 {
    type Output = Self;
    #[inline]
    fn add(self, other: f32) -> Vec3 {
        Self(self.0 + other, self.1 + other, self.2 + other)
    }
}

impl Sub<f32> for Vec3 {
    type Output = Self;
    #[inline]
    fn sub(self, other: f32) -> Vec3 {
        Self(self.0 - other, self.1 - other, self.2 - other)
    }
}

impl Mul<Vec3> for f32 {
    type Output = Vec3;
    #[inline]
    fn mul(self, other: Vec3) -> Vec3 {
        Vec3(self * other.0, self * other.1, self * other.2)
    }
}

impl Add<Vec3> for f32 {
    type Output = Vec3;
    #[inline]
    fn add(self, other: Vec3) -> Vec3 {
        Vec3(self + other.0, self + other.1, self + other.2)
    }
}

impl Sub<Vec3> for f32 {
    type Output = Vec3;
    #[inline]
    fn sub(self, other: Vec3) -> Vec3 {
        Vec3(self - other.0, self - other.1, self - other.2)
    }
}

fn smoothstep(edge0: f32, edge1: f32, x: f32) -> f32 {
    // Scale, bias and saturate x to 0..1 range
    let x = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    // Evaluate polynomial
    return x * x * (3.0 - 2.0 * x);
}

impl Mul<Vec3> for Vec3 {
    type Output = Vec3;
    #[inline]
    fn mul(self, other: Vec3) -> Vec3 {
        Vec3(self.0 * other.0, self.1 * other.1, self.2 * other.2)
    }
}

impl MulAssign<Vec3> for Vec3 {
    #[inline]
    fn mul_assign(&mut self, other: Vec3) {
        self.0 *= other.0;
        self.1 *= other.1;
        self.2 *= other.2;
    }
}

impl AddAssign<Vec3> for Vec3 {
    #[inline]
    fn add_assign(&mut self, other: Vec3) {
        self.0 += other.0;
        self.1 += other.1;
        self.2 += other.2;
    }
}

impl Add<Vec3> for Vec3 {
    type Output = Vec3;
    #[inline]
    fn add(self, other: Vec3) -> Vec3 {
        Vec3(self.0 + other.0, self.1 + other.1, self.2 + other.2)
    }
}

impl Div<Vec3> for Vec3 {
    type Output = Vec3;
    #[inline]
    fn div(self, other: Vec3) -> Vec3 {
        Vec3(self.0 / other.0, self.1 / other.1, self.2 / other.2)
    }
}

impl Neg for Vec3 {
    type Output = Vec3;
    #[inline]
    fn neg(self) -> Vec3 {
        Vec3(-self.0, -self.1, -self.2)
    }
}

impl Div<Vec3> for f32 {
    type Output = Vec3;
    #[inline]
    fn div(self, other: Vec3) -> Vec3 {
        Vec3(self / other.0, self / other.1, self / other.2)
    }
}

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

use core::f32::consts::PI;

fn pow(base: f32, factor: f32) -> f32 {
    unsafe { core::intrinsics::powf32(base, factor) }
}

fn sqrt(f: f32) -> f32 {
    unsafe { core::intrinsics::sqrtf32(f) }
}

fn log2(f: f32) -> f32 {
    unsafe { core::intrinsics::log2f32(f) }
}

fn abs(f: f32) -> f32 {
    unsafe { core::intrinsics::fabsf32(f) }
}

fn acos(v: f32) -> f32 {
    let x = abs(v);
    let mut res = -0.155972 * x + 1.56467; // p(x)
    res *= sqrt(1.0f32 - x);

    let mask = (v >= 0.0) as u32 as f32;

    // can't use if-statement so do oldskool shader masking instead to avoid conditional
    (res * mask) + ((1.0f32 - mask) * (PI - res))
}

fn cos(n: f32) -> f32 {
    unsafe { core::intrinsics::cosf32(n) }
}

fn normalize(v: Vec3) -> Vec3 {
    let len = 1.0 / sqrt(dot(v, v));
    Vec3::new(v.0 * len, v.1 * len, v.2 * len)
}

fn dot(a: Vec3, b: Vec3) -> f32 {
    a.0 * b.0 + a.1 * b.1 + a.2 * b.2
}

fn total_rayleigh(lambda: Vec3) -> Vec3 {
    (8.0 * pow(PI, 3.0)
        * pow(pow(REFRACTIVE_INDEX, 2.0) - 1.0, 2.0)
        * (6.0 + 3.0 * DEPOLARIZATION_FACTOR))
        / (3.0 * NUM_MOLECULES * lambda.pow(4.0) * (6.0 - 7.0 * DEPOLARIZATION_FACTOR))
}

fn total_mie(lambda: Vec3, k: Vec3, t: f32) -> Vec3 {
    let c = 0.2 * t * 10e-18;
    0.434 * c * PI * ((2.0 * PI) / lambda).pow(MIE_V - 2.0) * k
}

fn rayleigh_phase(cos_theta: f32) -> f32 {
    (3.0 / (16.0 * PI)) * (1.0 + pow(cos_theta, 2.0))
}

fn henyey_greenstein_phase(cos_theta: f32, g: f32) -> f32 {
    (1.0 / (4.0 * PI)) * ((1.0 - pow(g, 2.0)) / pow(1.0 - 2.0 * g * cos_theta + pow(g, 2.0), 1.5))
}

fn sun_intensity(zenith_angle_cos: f32) -> f32 {
    let cutoff_angle = PI / 1.95; // Earth shadow hack
    SUN_INTENSITY_FACTOR
        * 0.0f32.max(
            1.0 - exp(-((cutoff_angle - acos(zenith_angle_cos)) / SUN_INTENSITY_FALLOFF_STEEPNESS)),
        )
}

fn uncharted2_tonemap(w: Vec3) -> Vec3 {
    const A: f32 = 0.15; // Shoulder strength
    const B: f32 = 0.50; // Linear strength
    const C: f32 = 0.10; // Linear angle
    const D: f32 = 0.20; // Toe strength
    const E: f32 = 0.02; // Toe numerator
    const F: f32 = 0.30; // Toe denominator

    ((w * (A * w + C * B) + D * E) / (w * (A * w + B) + D * F)) - E / F
}

fn clamp(a: f32, b: f32, c: f32) -> f32 {
    a.min(b).max(c)
}

fn exp(a: f32) -> f32 {
    unsafe { core::intrinsics::expf32(a) }
}

fn exp_v3(a: Vec3) -> Vec3 {
    Vec3::new(exp(a.0), exp(a.1), exp(a.2))
}

fn lerp(a: Vec3, b: Vec3, c: f32) -> Vec3 {
    a * c + b * (1.0 - c)
}

fn sky(dir: Vec3, sun_position: Vec3) -> Vec3 {
    let up = Vec3::new(0.0, 1.0, 0.0);
    let sunfade = 1.0 - clamp(1.0 - exp(sun_position.1 / 450000.0), 0.0, 1.0);
    let rayleigh_coefficient = RAYLEIGH - (1.0 * (1.0 - sunfade));
    let beta_r = total_rayleigh(PRIMARIES) * rayleigh_coefficient;

    // Mie coefficient
    let beta_m = total_mie(PRIMARIES, MIE_K_COEFFICIENT, TURBIDITY) * MIE_COEFFICIENT;

    // Optical length, cutoff angle at 90 to avoid singularity
    let zenith_angle = acos(dot(up, dir).max(0.0));
    let denom = cos(zenith_angle) + 0.15 * pow(93.885 - ((zenith_angle * 180.0) / PI), -1.253);
    let s_r = RAYLEIGH_ZENITH_LENGTH / denom;
    let s_m = MIE_ZENITH_LENGTH / denom;

    // Combined extinction factor
    let fex = exp_v3(-(beta_r * s_r + beta_m * s_m));

    // In-scattering
    let sun_direction = normalize(sun_position);
    let cos_theta = dot(dir, sun_direction);
    let beta_r_theta = beta_r * rayleigh_phase(cos_theta * 0.5 + 0.5);

    let beta_m_theta = beta_m * henyey_greenstein_phase(cos_theta, MIE_DIRECTIONAL_G);
    let sun_e = sun_intensity(dot(sun_direction, up));
    let mut lin =
        (sun_e * ((beta_r_theta + beta_m_theta) / (beta_r + beta_m)) * (1.0 - fex)).pow(1.5);
    lin *= lerp(
        Vec3::splat(1.0),
        (sun_e * ((beta_r_theta + beta_m_theta) / (beta_r + beta_m)) * fex).pow(0.5),
        clamp(pow(1.0 - dot(up, sun_direction), 5.0), 0.0, 1.0),
    );

    // Composition + solar disc
    let sun_angular_diameter_cos = cos(SUN_ANGULAR_DIAMETER_DEGREES);
    let sundisk = smoothstep(
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
    let curr = uncharted2_tonemap((log2(2.0 / pow(LUMINANCE, 4.0))) * tex_color);
    let color = curr * white_scale;

    color.pow(1.0 / (1.2 + (1.2 * sunfade)))
}

#[allow(unused_attributes)]
#[spirv(entry = "fragment")]
pub fn main(input: Input<f32x4>, mut output: Output<f32x4>) {
    let color = input.load();

    let k = sky(Vec3::new(0.0, 1.0, 0.0), Vec3::new(0.0, -1.0, 0.0)); // todo: nice input variables

    output.store(f32x4(k.0, k.1, k.2, 0.0))
}

#[allow(unused_attributes)]
#[spirv(entry = "vertex")]
pub fn main_vs(
    in_pos: Input<f32x4>,
    in_color: Input<f32x4>,
    #[spirv(builtin = "position")] mut out_pos: Output<f32x4>,
    mut out_color: Output<f32x4>,
) {
    out_pos.store(in_pos.load());
    out_color.store(in_color.load());
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
