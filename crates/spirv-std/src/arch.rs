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
#[cfg(target_arch = "spirv")]
use core::arch::asm;

pub mod atomics;
mod barrier;
mod demote_to_helper_invocation_ext;
mod derivative;
mod primitive;
mod ray_tracing;

pub use atomics::*;
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
            "%bool = OpTypeBool",
            "%vector = OpLoad _ {vector}",
            "%result = OpAny %bool %vector",
            "OpStore {result} %result",
            vector = in(reg) &vector,
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
            "%bool = OpTypeBool",
            "%vector = OpLoad _ {vector}",
            "%result = OpAll %bool %vector",
            "OpStore {result} %result",
            vector = in(reg) &vector,
            result = in(reg) &mut result
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

/// Index into an array without bounds checking.
///
/// The main purpose of this trait is to work around the fact that the regular `get_unchecked*`
/// methods do not work in in SPIR-V.
pub trait IndexUnchecked<T> {
    /// Returns a reference to the element at `index`. The equivalent of `get_unchecked`.
    ///
    /// # Safety
    /// Behavior is undefined if the `index` value is greater than or equal to the length of the array.
    unsafe fn index_unchecked(&self, index: usize) -> &T;
    /// Returns a mutable reference to the element at `index`. The equivalent of `get_unchecked_mut`.
    ///
    /// # Safety
    /// Behavior is undefined if the `index` value is greater than or equal to the length of the array.
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T;
}

impl<T> IndexUnchecked<T> for [T] {
    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        asm!(
            "%slice_ptr = OpLoad _ {slice_ptr_ptr}",
            "%data_ptr = OpCompositeExtract _ %slice_ptr 0",
            "%val_ptr = OpAccessChain _ %data_ptr {index}",
            "OpReturnValue %val_ptr",
            slice_ptr_ptr = in(reg) &self,
            index = in(reg) index,
            options(noreturn)
        )
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        self.get_unchecked(index)
    }

    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        asm!(
            "%slice_ptr = OpLoad _ {slice_ptr_ptr}",
            "%data_ptr = OpCompositeExtract _ %slice_ptr 0",
            "%val_ptr = OpAccessChain _ %data_ptr {index}",
            "OpReturnValue %val_ptr",
            slice_ptr_ptr = in(reg) &self,
            index = in(reg) index,
            options(noreturn)
        )
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        self.get_unchecked_mut(index)
    }
}

impl<T, const N: usize> IndexUnchecked<T> for [T; N] {
    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        asm!(
            "%val_ptr = OpAccessChain _ {array_ptr} {index}",
            "OpReturnValue %val_ptr",
            array_ptr = in(reg) self,
            index = in(reg) index,
            options(noreturn)
        )
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        self.get_unchecked(index)
    }

    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        asm!(
            "%val_ptr = OpAccessChain _ {array_ptr} {index}",
            "OpReturnValue %val_ptr",
            array_ptr = in(reg) self,
            index = in(reg) index,
            options(noreturn)
        )
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        self.get_unchecked_mut(index)
    }
}
