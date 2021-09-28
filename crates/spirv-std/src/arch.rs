//! SPIR-V Instrinics
//!
//! This module is intended as a low level abstraction over SPIR-V instructions.
//! These functions will typically map to a single instruction, and will perform
//! no additional safety checks beyond type-checking.
use crate::{scalar::Scalar, vector::Vector};

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

/// Read from the shader clock.
///
/// Requires the `SPV_KHR_shader_clock` extension and the `ShaderClockKHR` capability.
///
/// See:
/// <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_shader_clock.html>
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub unsafe fn read_clock_khr() -> u64 {
    let mut result: u64 = 0;

    asm! {
        "%uint = OpTypeInt 32 0",
        "%scope = OpConstant %uint {scope}",
        "%result = OpReadClockKHR typeof*{result} %scope",
        "OpStore {result} %result",
        result = in(reg) &mut result,
        scope = const crate::memory::Scope::Subgroup as _,
    };

    result
}
