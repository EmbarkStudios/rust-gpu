//! Traits and helper functions related to floats.

use crate::vector::Vector;
#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Abstract trait representing a SPIR-V floating point type.
///
/// # Safety
/// Implementing this trait on non-primitive-float types breaks assumptions of other unsafe code,
/// and should not be done.
pub unsafe trait Float: num_traits::Float + crate::scalar::Scalar + Default {
    /// Width of the float, in bits.
    const WIDTH: usize;
}

unsafe impl Float for f32 {
    const WIDTH: usize = 32;
}

unsafe impl Float for f64 {
    const WIDTH: usize = 64;
}

/// Converts two f32 values (floats) into two f16 values (halfs). The result is a u32, with the low
/// 16 bits being the first f16, and the high 16 bits being the second f16.
#[spirv_std_macros::gpu_only]
pub fn vec2_to_f16x2(vec: impl Vector<f32, 2>) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 58 = PackHalf2x16
            "{result} = OpExtInst %uint %glsl 58 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Converts two f16 values (halfs) into two f32 values (floats). The parameter is a u32, with the
/// low 16 bits being the first f16, and the high 16 bits being the second f16.
#[spirv_std_macros::gpu_only]
pub fn f16x2_to_vec2<V: Vector<f32, 2>>(int: u32) -> V {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%float = OpTypeFloat 32",
            "%vec2 = OpTypeVector %float 2",
            // 62 = UnpackHalf2x16
            "%result = OpExtInst %vec2 %glsl 62 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

// We don't have access to a concrete vector type (cfg(feature = "glam") might not be enabled), so
// synth up one manually.
#[cfg_attr(target_arch = "spirv", repr(simd))]
// sometimes dead because on cpu, the `gpu_only` macro nukes the method bodies
#[allow(dead_code)]
#[derive(Default)]
struct F32x2 {
    x: f32,
    y: f32,
}
unsafe impl Vector<f32, 2> for F32x2 {}

/// Converts an f32 (float) into an f16 (half). The result is a u32, not a u16, due to GPU support
/// for u16 not being universal - the upper 16 bits will always be zero.
#[spirv_std_macros::gpu_only]
pub fn f32_to_f16(float: f32) -> u32 {
    vec2_to_f16x2(F32x2 { x: float, y: 0.0 })
}

/// Converts an f16 (half) into an f32 (float). The parameter is a u32, due to GPU support for u16
/// not being universal - the upper 16 bits are ignored.
#[spirv_std_macros::gpu_only]
pub fn f16_to_f32(packed: u32) -> f32 {
    f16x2_to_vec2::<F32x2>(packed).x
}

/// Packs a vec4 into 4 8-bit signed integers. See
/// [PackSnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec4_to_u8x4_snorm(vec: impl Vector<f32, 4>) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 54 = PackSnorm4x8
            "{result} = OpExtInst %uint %glsl 54 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Packs a vec4 into 4 8-bit unsigned integers. See
/// [PackUnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec4_to_u8x4_unorm(vec: impl Vector<f32, 4>) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 55 = PackUnorm4x8
            "{result} = OpExtInst %uint %glsl 55 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Packs a vec2 into 2 16-bit signed integers. See
/// [PackSnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec2_to_u16x2_snorm(vec: impl Vector<f32, 2>) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 56 = PackSnorm2x16
            "{result} = OpExtInst %uint %glsl 56 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Packs a vec2 into 2 16-bit unsigned integers. See
/// [PackUnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec2_to_u16x2_unorm(vec: impl Vector<f32, 2>) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 57 = PackUnorm2x16
            "{result} = OpExtInst %uint %glsl 57 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Unpacks 4 8-bit signed integers into a vec4. See
/// [UnpackSnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn u8x4_to_vec4_snorm<V: Vector<f32, 4>>(int: u32) -> V {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%float = OpTypeFloat 32",
            "%vec4 = OpTypeVector %float 4",
            // 63 = UnpackSnorm4x8
            "%result = OpExtInst %vec4 %glsl 63 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

/// Unpacks 4 8-bit unsigned integers into a vec4. See
/// [UnpackSnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn u8x4_to_vec4_unorm<V: Vector<f32, 4>>(int: u32) -> V {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%float = OpTypeFloat 32",
            "%vec4 = OpTypeVector %float 4",
            // 64 = UnpackUnorm4x8
            "%result = OpExtInst %vec4 %glsl 64 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

/// Unpacks 2 16-bit signed integers into a vec2. See
/// [UnpackSnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for
/// exact semantics.
#[spirv_std_macros::gpu_only]
pub fn u16x2_to_vec2_snorm<V: Vector<f32, 2>>(int: u32) -> V {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%float = OpTypeFloat 32",
            "%vec2 = OpTypeVector %float 2",
            // 60 = UnpackSnorm2x16
            "%result = OpExtInst %vec2 %glsl 60 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

/// Unpacks 2 16-bit unsigned integers into a vec2. See
/// [UnpackUnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for
/// exact semantics.
#[spirv_std_macros::gpu_only]
pub fn u16x2_to_vec2_unorm<V: Vector<f32, 2>>(int: u32) -> V {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%float = OpTypeFloat 32",
            "%vec2 = OpTypeVector %float 2",
            // 61 = UnpackUnorm2x16
            "%result = OpExtInst %vec2 %glsl 61 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}
