// The new preferred style is still to use `unsafe` blocks in `unsafe` functions
// but the compiler/clippy hasn't caught up to that style yet, so we just
// disable the lint.
#![allow(unused_unsafe)]

use crate::{
    float::Float,
    integer::{Integer, SignedInteger, UnsignedInteger},
    vector::Vector,
};

/// Signed-integer subtract of `operand` from zero. Results are computed
/// per component.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpSNegate")]
#[inline]
pub fn s_negate_vector<S, V, const N: usize>(operand: V) -> V
where
    S: SignedInteger,
    V: Vector<S, N>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%operand = OpLoad typeof*{operand} {operand}",
            "%result = OpSNegate typeof*{operand} %operand",
            "OpStore {result} %result",
            operand = in(reg) &operand,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Floating-point subtract of `operand` from zero. Results are computed
/// per component.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFNegate")]
#[inline]
pub fn f_negate_vector<F, V, const N: usize>(operand: V) -> V
where
    F: Float,
    V: Vector<F, N>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%operand = OpLoad typeof*{operand} {operand}",
            "%result = OpFNegate typeof*{operand} %operand",
            "OpStore {result} %result",
            operand = in(reg) &operand,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Integer addition of `x` and `y`. Results are computed per component.
///
/// # Safety
/// The resulting value is undefined if the computation would result
/// in overflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpIAdd")]
#[inline]
pub unsafe fn i_add_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: Integer,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpIAdd typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Floating-point addition of `x` and `y`. Results are computed per component.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFAdd")]
#[inline]
pub fn f_add_vector<F, V, const LEN: usize>(x: V, y: V) -> V
where
    F: Float,
    V: Vector<F, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpFAdd typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Signed-integer subtract of `x` from `y`. Results are computed per component.
///
/// # Safety
/// The resulting value is undefined if the computation would result
/// in underflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpISub")]
#[inline]
pub fn i_sub_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: Integer,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpISub typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Floating-point subtract of `x` from `y`. Results are computed per component.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFSub")]
#[inline]
pub fn f_sub_vector<F, V, const LEN: usize>(x: V, y: V) -> V
where
    F: Float,
    V: Vector<F, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpFSub typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Integer multiplication of `x` and `y`. Results are computed per component.
///
/// # Safety
/// The resulting value is undefined if the computation would result
/// in underflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpIMul")]
#[inline]
pub fn i_mul_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: Integer,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpIMul typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Floating-point multiplication of `x` and `y`. Results are computed
/// per component.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFMul")]
#[inline]
pub fn f_mul_vector<F, V, const LEN: usize>(x: V, y: V) -> V
where
    F: Float,
    V: Vector<F, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpFMul typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Unsigned-integer division of `x` from `y`.  Results are computed
/// per component.
///
/// # Safety
/// The resulting value is undefined if any component of `y` is `0`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpUDiv")]
#[inline]
pub unsafe fn u_div_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: UnsignedInteger,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpUDiv typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Signed-integer division of `x` from `y`. Results are computed
/// per component.
///
/// # Safety
/// The resulting value is undefined if any component of `y` is `0`, or if a
/// component of `y` is `-1` and the dividing component of `x` is
/// minimum representable value for its type, causing signed overflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpSDiv")]
#[inline]
pub unsafe fn s_div_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: SignedInteger,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpSDiv typeof*{y} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Floating-point division of `x` from `y`. Results are computed
/// per component.
///
/// # Safety
/// The resulting value is undefined if any component of `y` is `0.0`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFDiv")]
#[inline]
pub fn f_div_vector<F, V, const LEN: usize>(x: V, y: V) -> V
where
    F: Float,
    V: Vector<F, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpFDiv typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Unsigned modulo operation of `x` modulo `y`. Results are computed
/// per component.
///
/// # Safety
/// The resulting value is undefined if `y` is `0`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpUMod")]
#[inline]
pub fn u_mod_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: UnsignedInteger,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpUMod typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Signed-integer remainder operation for getting the remainder from `x / y`
/// whose sign matches the sign of `x`.
///
/// # Safety
/// Behavior is undefined if `y` is 0, or if `y` is -1 and `x` is the minimum
/// representable value for the type, causing signed overflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpSRem")]
#[inline]
pub fn s_rem_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: SignedInteger,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpSRem typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Signed-integer modulo operation from `x` modulo `y`, whose sign matches the
/// sign of `y`. Results are computed per component.
///
/// # Safety
/// Behavior is undefined if `y` is 0, or if `y` is -1 and `x` is the minimum
/// representable value for the type, causing signed overflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpSMod")]
#[inline]
pub fn s_mod_vector<I, V, const LEN: usize>(x: V, y: V) -> V
where
    I: SignedInteger,
    V: Vector<I, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpSMod typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Floating-point remainder operation for getting the remainder from `x / y`
/// whose sign matches the sign of `x`. Results are computed per component.
///
/// # Safety
/// Behavior is undefined if `y` is 0, or if `y` is -1 and `x` is the minimum
/// representable value for the type, causing signed overflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFRem")]
#[inline]
pub fn f_rem_vector<F, V, const LEN: usize>(x: V, y: V) -> V
where
    F: Float,
    V: Vector<F, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpFRem typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Floating-point modulo operation from `x` modulo `y`, whose sign matches the
/// sign of `y`. Results are computed per component.
///
/// # Safety
/// Behavior is undefined if `y` is 0, or if `y` is -1 and `x` is the minimum
/// representable value for the type, causing signed overflow.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpFMod")]
#[inline]
pub fn f_mod_vector<F, V, const LEN: usize>(x: V, y: V) -> V
where
    F: Float,
    V: Vector<F, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%x = OpLoad typeof*{x} {x}",
            "%y = OpLoad typeof*{y} {y}",
            "%result = OpFMod typeof*{x} %x %y",
            "OpStore {result} %result",
            x = in(reg) &x,
            y = in(reg) &y,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Scale a floating-point `vector` by `scalar`. Each component of `vector` is
/// multiplied by `scalar`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpVectorTimesScalar")]
#[inline]
pub fn vector_times_scalar<F, V, const LEN: usize>(vector: V, scalar: F) -> V
where
    F: Float,
    V: Vector<F, LEN>,
{
    let mut result = V::default();

    unsafe {
        asm! {
            "%vector = OpLoad typeof*{vector} {vector}",
            "%scalar = OpLoad typeof*{scalar} {scalar}",
            "%result = OpVectorTimesScalar typeof*{vector} %vector %scalar",
            "OpStore {result} %result",
            vector = in(reg) &vector,
            scalar = in(reg) &scalar,
            result = in(reg) &mut result,
        }
    }

    result
}
