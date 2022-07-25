//! SPIR-V Atomics
//!

use crate::{
    integer::{Integer, SignedInteger, UnsignedInteger},
    number::Number,
};

#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Atomically load through `ptr` using the given `SEMANTICS`. All subparts of
/// the value that is loaded are read atomically with respect to all other
/// atomic accesses to it within `SCOPE`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicLoad")]
#[inline]
pub unsafe fn atomic_load<N: Number, const SCOPE: u8, const SEMANTICS: u8>(ptr: &N) -> N {
    let mut result = N::default();

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

/// Atomically store through `ptr` using the given `SEMANTICS`. All subparts of
/// `value` are written atomically with respect to all other atomic accesses to
/// it within `SCOPE`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicStore")]
#[inline]
pub unsafe fn atomic_store<N: Number, const SCOPE: u8, const SEMANTICS: u8>(ptr: &mut N, value: N) {
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%value = OpLoad _ {value}",
        "OpAtomicStore {ptr} %scope %semantics %value",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        value = in(reg) &value,
    }
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within `SCOPE` to the same location:
///
/// 1. Load through `ptr` to get the original value,
/// 2. Get a new value from copying `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicExchange")]
#[inline]
pub unsafe fn atomic_exchange<N: Number, const SCOPE: u8, const SEMANTICS: u8>(
    ptr: &mut N,
    value: N,
) -> N {
    let mut old = N::default();

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
        value = in(reg) &value,
        old = in(reg) &mut old,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within `SCOPE` to the same location:
///
/// 1. Load through `ptr` to get the original value
/// 2. Get a new value from `value` only if the original value equals
///    `comparator`, and
/// 3. Store the new value back through `ptr`, only if the original value
///    equaled `comparator`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicCompareExchange")]
#[inline]
pub unsafe fn atomic_compare_exchange<
    I: Integer,
    const SCOPE: u8,
    const EQUAL: u8,
    const UNEQUAL: u8,
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

/// Perform the following steps atomically with respect to any other atomic
/// accesses within `SCOPE` to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value through integer addition of 1 to original value, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIIncrement")]
pub unsafe fn atomic_i_increment<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
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
        old = in(reg) &mut old,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within `SCOPE` to the same location:
///
/// 1) load through `ptr` to get an original value,
/// 2) get a new value through integer subtraction of 1 from original value, and
/// 3) store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIDecrement")]
#[inline]
pub unsafe fn atomic_i_decrement<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
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
        old = in(reg) &mut old,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within `SCOPE` to the same location:
///
/// 1) load through `ptr` to get an original value,
/// 2) get a new value by integer addition of original value and `value`, and
/// 3) store the new value back through `ptr`.
///
/// The result is the Original Value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIAdd")]
#[inline]
pub unsafe fn atomic_i_add<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within `SCOPE` to the same location:
///
/// 1) load through `ptr` to get an original value,
/// 2) get a new value by integer subtraction of original value and `value`, and
/// 3) store the new value back through `ptr`.
///
/// The result is the Original Value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicISub")]
#[inline]
pub unsafe fn atomic_i_sub<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within Scope to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value by finding the smallest signed integer of original value
///    and `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicSMin")]
#[inline]
pub unsafe fn atomic_s_min<I: SignedInteger, const SCOPE: u8, const SEMANTICS: u8>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within Scope to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value by finding the smallest unsigned integer of original
///    value and `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicUMin")]
#[inline]
pub unsafe fn atomic_u_min<I: UnsignedInteger, const SCOPE: u8, const SEMANTICS: u8>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within Scope to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value by finding the largest signed integer of original value
///    and `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicSMax")]
#[inline]
pub unsafe fn atomic_s_max<I: SignedInteger, const SCOPE: u8, const SEMANTICS: u8>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within Scope to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value by finding the largest unsigned integer of original
///    value and `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicUMax")]
#[inline]
pub unsafe fn atomic_u_max<I: UnsignedInteger, const SCOPE: u8, const SEMANTICS: u8>(
    ptr: &mut I,
    value: I,
) -> I {
    let mut old = I::default();

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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within Scope to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value by the bitwise AND of the original value and `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicAnd")]
#[inline]
pub unsafe fn atomic_and<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within Scope to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value by the bitwise OR of the original value and `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicOr")]
#[inline]
pub unsafe fn atomic_or<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
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
        value = in(reg) &value,
    }

    old
}

/// Perform the following steps atomically with respect to any other atomic
/// accesses within Scope to the same location:
///
/// 1. Load through `ptr` to get an original value,
/// 2. Get a new value by the bitwise XOR of the original value and `value`, and
/// 3. Store the new value back through `ptr`.
///
/// The result is the original value.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicXor")]
#[inline]
pub unsafe fn atomic_xor<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
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
        value = in(reg) &value,
    }

    old
}

/// Atomically sets the flag value pointed to by Pointer to the set state.
/// Pointer must be a pointer to a 32-bit integer type representing an atomic flag.
/// The instruction’s result is true if the flag was in the set state or false if the flag was in the clear state immediately before the operation.
/// **Important:** Kernel capabilities have to be set beforehand.
///
/// Result Type must be a Boolean type.
/// The resulting values are undefined if an atomic flag is modified by an instruction other than OpAtomicFlagTestAndSet or OpAtomicFlagClear.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFlagTestAndSet")]
#[inline]
pub unsafe fn atomic_flag_test_and_set<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(
    ptr: &mut I,
) -> bool {
    let mut old: bool = false;

    asm! {
        "%bool = OpTypeBool",
        "%u32 = OpTypeInt 32 0",
        "%scope = OpConstant %u32 {scope}",
        "%semantics = OpConstant %u32 {semantics}",
        "%old = OpAtomicFlagTestAndSet %bool {ptr} %scope %semantics",
        "OpStore {old} %old",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
        old = in(reg) &mut old,
    }

    old
}

/// Atomically sets the flag value pointed to by Pointer to the clear state.
///
/// Pointer must be a pointer to a 32-bit integer type representing an atomic flag.
/// **Important:** Memory Semantics must not be Acquire or AcquireRelease
///
/// The resulting values are undefined if an atomic flag is modified by an instruction other than OpAtomicFlagTestAndSet or OpAtomicFlagClear.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFlagClear")]
#[inline]
pub unsafe fn atomic_flag_clear<I: Integer, const SCOPE: u8, const SEMANTICS: u8>(ptr: &mut I) {
    // Ensure the memory semantic is not Acquire or AcquireRelease
    assert!(
        SEMANTICS
            != (crate::memory::Semantics::ACQUIRE.bits() as u8
                | crate::memory::Semantics::ACQUIRE_RELEASE.bits() as u8)
    );
    asm! {
        "%u32 = OpTypeInt 32 0",
        "%semantics = OpConstant %u32 {semantics}",
        "%scope = OpConstant %u32 {scope}",
        "OpAtomicFlagClear {ptr} %scope %semantics",
        scope = const SCOPE,
        semantics = const SEMANTICS,
        ptr = in(reg) ptr,
    }
}
