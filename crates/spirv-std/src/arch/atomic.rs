#[cfg(target_arch = "spirv")]
use core::arch::asm;

use crate::{
    float::Float,
    integer::{Integer, SignedInteger, UnsignedInteger},
    scalar::Scalar,
};

/// Atomically load a scalar and return the value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicLoad")]
#[inline]
pub unsafe fn atomic_load<T: Scalar, const SCOPE: u32, const SEMANTICS: u32>(ptr: &T) -> T {
    let mut result = T::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%result = OpAtomicLoad _ {ptr} %scope %semantics",
        "OpStore {result} %result",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        result = in(reg) &mut result
    }

    result
}

/// Atomically store a scalar.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicStore")]
#[inline]
pub unsafe fn atomic_store<T: Scalar, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut T,
    value: T,
) {
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "OpAtomicStore {ptr} %scope %semantics %value",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        value = in(reg) &value
    }
}

/// Atomically exchange a scalar and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicExchange")]
#[inline]
pub unsafe fn atomic_exchange<T: Scalar, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut T,
    value: T,
) -> T {
    let mut old = T::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicExchange _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically compare and exchange an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicCompareExchange")]
#[inline]
pub unsafe fn atomic_compare_exchange<
    I: Integer,
    const SCOPE: u32,
    const EQUAL: u32,
    const UNEQUAL: u32,
>(
    ptr: &mut I,
    value: I,
    comparator: I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%equal = OpConstant %u32 {equal}",
        "%unequal = OpConstant %u32 {unequal}",
        "%value = OpLoad _ {value}",
        "%comparator = OpLoad _ {comparator}",
        "%old = OpAtomicCompareExchange _ {ptr} %scope %equal %unequal %value %comparator",
        "OpStore {old} %old",
        scope = const SCOPE,
        equal = const EQUAL,
        unequal = const UNEQUAL,
        ptr = in(reg) ptr,
        value = in(reg) &value,
        comparator = in(reg) &comparator,
        old = in(reg) &mut old,
    }

    old
}

/// Atomically increment an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIIncrement")]
#[inline]
pub unsafe fn atomic_i_increment<I: Integer, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%old = OpAtomicIIncrement _ {ptr} %scope %semantics",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old
    }

    old
}

/// Atomically decrement an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIDecrement")]
#[inline]
pub unsafe fn atomic_i_decrement<I: Integer, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%old = OpAtomicIDecrement _ {ptr} %scope %semantics",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old
    }

    old
}

/// Atomically add an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIAdd")]
#[inline]
pub unsafe fn atomic_i_add<I: Integer, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicIAdd _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically sub an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicISub")]
#[inline]
pub unsafe fn atomic_i_sub<I: Integer, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicISub _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically min a signed integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicSMin")]
#[inline]
pub unsafe fn atomic_s_min<S: SignedInteger, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut S,
    value: S,
) -> S {
    let mut old = S::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicSMin _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically min an unsigned integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicUMin")]
#[inline]
pub unsafe fn atomic_u_min<U: UnsignedInteger, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut U,
    value: U,
) -> U {
    let mut old = U::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicUMin _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically max a signed integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicSMax")]
#[inline]
pub unsafe fn atomic_s_max<S: SignedInteger, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut S,
    value: S,
) -> S {
    let mut old = S::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicSMax _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically max an unsigned integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicUMax")]
#[inline]
pub unsafe fn atomic_u_max<U: UnsignedInteger, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut U,
    value: U,
) -> U {
    let mut old = U::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicUMax _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically and an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicAnd")]
#[inline]
pub unsafe fn atomic_and<I: Integer, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicAnd _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically or an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicOr")]
#[inline]
pub unsafe fn atomic_or<I: Integer, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicOr _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically xor an integer and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicXor")]
#[inline]
pub unsafe fn atomic_xor<I: Integer, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicXor _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically min a float and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFMinEXT")]
#[inline]
pub unsafe fn atomic_f_min<F: Float, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut F,
    value: F,
) -> F {
    let mut old = F::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicFMinEXT _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically max a float and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFMaxEXT")]
#[inline]
pub unsafe fn atomic_f_max<F: Float, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut F,
    value: F,
) -> F {
    let mut old = F::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicFMaxEXT _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}

/// Atomically add a float and return the old value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFAddEXT")]
#[inline]
pub unsafe fn atomic_f_add<F: Float, const SCOPE: u32, const SEMANTICS: u32>(
    ptr: &mut F,
    value: F,
) -> F {
    let mut old = F::default();

    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "%old = OpAtomicFMaxEXT _ {ptr} %scope %semantics %value",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
        value = in(reg) &value
    }

    old
}
