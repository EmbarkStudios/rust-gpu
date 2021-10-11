//! SPIR-V Instrinics
//!
//! This module is intended as a low level abstraction over SPIR-V instructions.
//! These functions will typically map to a single instruction, and will perform
//! no additional safety checks beyond type-checking.
use crate::{
    integer::{Integer, SignedInteger, UnsignedInteger},
    scalar::Scalar,
    vector::Vector,
};

mod barrier;
mod demote_to_helper_invocation_ext;
mod derivative;
mod primitive;
mod ray_tracing;

pub use barrier::*;
pub use demote_to_helper_invocation_ext::*;
pub use derivative::*;
pub use primitive::*;
pub use ray_tracing::*;

/// Result is true if any component of `vector` is true, otherwise result is
/// false.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAny")]
#[inline]
pub fn any<V: Vector<bool, N>, const N: usize>(vector: V) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            // Types & Constants
            "%bool = OpTypeBool",
            "%u8 = OpTypeInt 8 0",
            "%u8_0 = OpConstant %u8 0",
            "%u8_1 = OpConstant %u8 1",
            "%glam_vec_type = OpTypeVector %u8 {len}",
            "%bool_vec_type = OpTypeVector %bool {len}",
            "%false_vec = OpConstantNull %glam_vec_type",
            // Code
            "%vector = OpLoad %glam_vec_type {vector}",
            "%bool_vec = OpINotEqual %bool_vec_type %vector %false_vec",
            "%result = OpAny %bool %bool_vec",
            "%boolean = OpSelect %u8 %result %u8_1 %u8_0",
            "OpStore {result} %boolean",
            vector = in(reg) &vector,
            len = const N,
            result = in(reg) &mut result
        }
    }

    result
}

/// Result is true if all components of `vector` is true, otherwise result is
/// false.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAll")]
#[inline]
pub fn all<V: Vector<bool, N>, const N: usize>(vector: V) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            // Types & Constants
            "%bool = OpTypeBool",
            "%u8 = OpTypeInt 8 0",
            "%u8_0 = OpConstant %u8 0",
            "%u8_1 = OpConstant %u8 1",
            "%glam_vec_type = OpTypeVector %u8 {len}",
            "%bool_vec_type = OpTypeVector %bool {len}",
            "%false_vec = OpConstantNull %glam_vec_type",
            // Code
            "%vector = OpLoad %glam_vec_type {vector}",
            "%bool_vec = OpINotEqual %bool_vec_type %vector %false_vec",
            "%result = OpAll %bool %bool_vec",
            "%boolean = OpSelect %u8 %result %u8_1 %u8_0",
            "OpStore {element} %boolean",
            vector = in(reg) &vector,
            len = const N,
            element = in(reg) &mut result
        }
    }

    result
}

/// Extract a single, dynamically selected, component of a vector.
///
/// # Safety
/// Behavior is undefined if `index`’s value is greater than or equal to the
/// number of components in `vector`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpVectorExtractDynamic")]
#[inline]
pub unsafe fn vector_extract_dynamic<T: Scalar, const N: usize>(
    vector: impl Vector<T, N>,
    index: usize,
) -> T {
    let mut result = T::default();

    asm! {
        "%vector = OpLoad _ {vector}",
        "%element = OpVectorExtractDynamic _ %vector {index}",
        "OpStore {element} %element",
        vector = in(reg) &vector,
        index = in(reg) index,
        element = in(reg) &mut result
    }

    result
}

/// Make a copy of a vector, with a single, variably selected,
/// component modified.
///
/// # Safety
/// Behavior is undefined if `index`’s value is greater than or equal to the
/// number of components in `vector`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpVectorInsertDynamic")]
#[inline]
pub unsafe fn vector_insert_dynamic<T: Scalar, V: Vector<T, N>, const N: usize>(
    vector: V,
    index: usize,
    element: T,
) -> V {
    let mut result = V::default();

    asm! {
        "%vector = OpLoad _ {vector}",
        "%element = OpLoad _ {element}",
        "%new_vector = OpVectorInsertDynamic _ %vector %element {index}",
        "OpStore {result} %new_vector",
        vector = in(reg) &vector,
        index = in(reg) index,
        element = in(reg) &element,
        result = in(reg) &mut result,
    }

    result
}

/// Fragment-shader discard. Equivalvent to `discard()` from GLSL
///
/// Ceases all further processing in any invocation that executes it: Only
/// instructions these invocations executed before [kill] have observable side
/// effects.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpKill", alias = "discard")]
#[allow(clippy::empty_loop)]
pub fn kill() -> ! {
    unsafe { asm!("OpKill", options(noreturn)) }
}

/// Read from the shader clock with either the `Subgroup` or `Device` scope.
///
/// See:
/// <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_shader_clock.html>
#[cfg(all(
    target_feature = "Int64",
    target_feature = "ShaderClockKHR",
    target_feature = "ext:SPV_KHR_shader_clock"
))]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub unsafe fn read_clock_khr<const SCOPE: u32>() -> u64 {
    let mut result: u64;

    asm! {
        "%uint = OpTypeInt 32 0",
        "%scope = OpConstant %uint {scope}",
        "{result} = OpReadClockKHR typeof*{result} %scope",
        result = out(reg) result,
        scope = const SCOPE,
    };

    result
}

/// Like `read_clock_khr` but returns a vector to avoid requiring the `Int64`
/// capability. It returns a 'vector of two-components of 32-bit unsigned
/// integer type with the first component containing the 32 least significant
/// bits and the second component containing the 32 most significant bits.'
#[cfg(all(
    target_feature = "ShaderClockKHR",
    target_feature = "ext:SPV_KHR_shader_clock"
))]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub unsafe fn read_clock_uvec2_khr<V: Vector<u32, 2>, const SCOPE: u32>() -> V {
    let mut result = V::default();

    asm! {
        "%uint = OpTypeInt 32 0",
        "%scope = OpConstant %uint {scope}",
        "%result = OpReadClockKHR typeof*{result} %scope",
        "OpStore {result} %result",
        result = in(reg) &mut result,
        scope = const SCOPE,
    };

    result
}

#[spirv_std_macros::gpu_only]
unsafe fn call_glsl_op_with_ints<T: Integer, const OP: u32>(a: T, b: T) -> T {
    let mut result = T::default();
    asm!(
        "%glsl = OpExtInstImport \"GLSL.std.450\"",
        "%a = OpLoad _ {a}",
        "%b = OpLoad _ {b}",
        "%result = OpExtInst typeof*{result} %glsl {op} %a %b",
        "OpStore {result} %result",
        a = in(reg) &a,
        b = in(reg) &b,
        result = in(reg) &mut result,
        op = const OP
    );
    result
}

/// Compute the minimum of two unsigned integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn unsigned_min<T: UnsignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 38>(a, b) }
}

/// Compute the maximum of two unsigned integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn unsigned_max<T: UnsignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 41>(a, b) }
}

/// Compute the minimum of two signed integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn signed_min<T: SignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 39>(a, b) }
}

/// Compute the maximum of two signed integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn signed_max<T: SignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 42>(a, b) }
}
