pub mod mat2;
pub mod mat3;
pub mod mat4;
/// This math library is heavily borrowed from https://github.com/bitshifter/glam-rs
pub mod vec2;
pub mod vec3;
pub mod vec4;
pub use mat2::*;
pub use mat3::*;
pub use mat4::*;
pub use vec2::*;
pub use vec3::*;
pub use vec4::*;

pub mod builtin {
    pub fn powf32(base: f32, factor: f32) -> f32 {
        unsafe { core::intrinsics::powf32(base, factor) }
    }

    pub fn sqrtf32(f: f32) -> f32 {
        unsafe { core::intrinsics::sqrtf32(f) }
    }

    pub fn log2f32(f: f32) -> f32 {
        unsafe { core::intrinsics::log2f32(f) }
    }

    pub fn absf32(f: f32) -> f32 {
        unsafe { core::intrinsics::fabsf32(f) }
    }

    pub fn cosf32(n: f32) -> f32 {
        unsafe { core::intrinsics::cosf32(n) }
    }

    pub fn roundf32(n: f32) -> f32 {
        unsafe { core::intrinsics::roundf32(n) }
    }

    pub fn floorf32(n: f32) -> f32 {
        unsafe { core::intrinsics::floorf32(n) }
    }

    pub fn ceilf32(n: f32) -> f32 {
        unsafe { core::intrinsics::ceilf32(n) }
    }

    pub fn expf32(n: f32) -> f32 {
        unsafe { core::intrinsics::expf32(n) }
    }
}
