//! This math library is heavily borrowed from https://github.com/bitshifter/glam-rs
//! In the future we hope to be able to use it directly!

pub mod mat2;
pub mod mat3;
pub mod mat4;
pub mod vec2;
pub mod vec3;
pub mod vec4;
pub use mat2::*;
pub use mat3::*;
pub use mat4::*;
pub use vec2::*;
pub use vec3::*;
pub use vec4::*;

pub trait MathExt {
    fn pow(self, factor: Self) -> Self;
    fn sqrt(self) -> Self;
    fn log2(self) -> Self;
    fn abs(self) -> Self;
    fn cos(self) -> Self;
    fn round(self) -> Self;
    fn floor(self) -> Self;
    fn ceil(self) -> Self;
    fn exp(self) -> Self;
    fn saturate(self) -> Self;
}

impl MathExt for f32 {
    fn pow(self, factor: f32) -> f32 {
        unsafe { core::intrinsics::powf32(self, factor) }
    }

    fn sqrt(self) -> f32 {
        unsafe { core::intrinsics::sqrtf32(self) }
    }

    fn log2(self) -> f32 {
        unsafe { core::intrinsics::log2f32(self) }
    }

    fn abs(self) -> f32 {
        unsafe { core::intrinsics::fabsf32(self) }
    }

    fn cos(self) -> f32 {
        unsafe { core::intrinsics::cosf32(self) }
    }

    fn round(self) -> f32 {
        unsafe { core::intrinsics::roundf32(self) }
    }

    fn floor(self) -> f32 {
        unsafe { core::intrinsics::floorf32(self) }
    }

    fn ceil(self) -> f32 {
        unsafe { core::intrinsics::ceilf32(self) }
    }

    fn exp(self) -> f32 {
        unsafe { core::intrinsics::expf32(self) }
    }

    fn saturate(self) -> f32 {
        self.max(0.0).min(1.0)
    }
}
