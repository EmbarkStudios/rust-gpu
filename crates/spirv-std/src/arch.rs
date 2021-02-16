//! SPIR-V Instrinics
//!
//! This module is intended as a low level abstraction over SPIR-V instructions.
//! These functions will typically map to a single instruction, and will perform
//! no additional safety checks beyond type-checking.
use crate::{scalar::Scalar, vector::Vector};

/// Extract a single, dynamically selected, component of a vector.
///
/// # Safety
/// Behavior is undefined if `index`’s value is greater than or equal to the
/// number of components in `vector`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpVectorExtractDynamic")]
#[inline]
pub unsafe fn vector_extract_dynamic<T: Scalar, V: Vector<T>>(vector: V, index: usize) -> T {
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
pub unsafe fn vector_insert_dynamic<T: Scalar, V: Vector<T>>(
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
