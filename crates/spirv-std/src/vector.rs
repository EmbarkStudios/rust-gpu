//! Traits related to vectors.

use crate::scalar::Scalar;

/// Abstract trait representing a SPIR-V vector type.
///
/// # Safety
/// Implementing this trait on non-simd-vector types breaks assumptions of other unsafe code, and
/// should not be done.
pub unsafe trait Vector<T: crate::scalar::Scalar, const N: usize>: Default {}

unsafe impl Vector<f32, 2> for glam::Vec2 {}
unsafe impl Vector<f32, 3> for glam::Vec3 {}
unsafe impl Vector<f32, 3> for glam::Vec3A {}
unsafe impl Vector<f32, 4> for glam::Vec4 {}

unsafe impl Vector<f64, 2> for glam::DVec2 {}
unsafe impl Vector<f64, 3> for glam::DVec3 {}
unsafe impl Vector<f64, 4> for glam::DVec4 {}

unsafe impl Vector<u32, 2> for glam::UVec2 {}
unsafe impl Vector<u32, 3> for glam::UVec3 {}
unsafe impl Vector<u32, 4> for glam::UVec4 {}

unsafe impl Vector<i32, 2> for glam::IVec2 {}
unsafe impl Vector<i32, 3> for glam::IVec3 {}
unsafe impl Vector<i32, 4> for glam::IVec4 {}

/// Abstract trait representing a SPIR-V scalar or vector type.
///
/// # Safety
/// Implementing this trait on non-simd-vector/scalar types breaks assumptions of other unsafe code, and
/// should not be done.
pub unsafe trait ScalarOrVector {
    /// The scalar type
    type Scalar;

    /// The dimension of the vector, or 1 if it is a scalar
    const DIM: usize;
}

unsafe impl<S: Scalar> ScalarOrVector for S {
    type Scalar = S;
    const DIM: usize = 1;
}

macro_rules! impl_vector_or_scalar {
    ($(for $scalar_ty:ty, $dim: literal: unsafe impl ScalarOrVector for $typ:ty;)+) => {
        $(
            unsafe impl ScalarOrVector for $typ {
                type Scalar = $scalar_ty;
                const DIM: usize = $dim;
            }
        )+
    }
}

impl_vector_or_scalar! {
    for bool, 2: unsafe impl ScalarOrVector for glam::BVec2;
    for bool, 3: unsafe impl ScalarOrVector for glam::BVec3;
    for bool, 4: unsafe impl ScalarOrVector for glam::BVec4;

    for f32, 2: unsafe impl ScalarOrVector for glam::Vec2;
    for f32, 3: unsafe impl ScalarOrVector for glam::Vec3;
    for f32, 4: unsafe impl ScalarOrVector for glam::Vec4;

    for f64, 2: unsafe impl ScalarOrVector for glam::DVec2;
    for f64, 3: unsafe impl ScalarOrVector for glam::DVec3;
    for f64, 4: unsafe impl ScalarOrVector for glam::DVec4;

    for u32, 2: unsafe impl ScalarOrVector for glam::UVec2;
    for u32, 3: unsafe impl ScalarOrVector for glam::UVec3;
    for u32, 4: unsafe impl ScalarOrVector for glam::UVec4;

    for i32, 2: unsafe impl ScalarOrVector for glam::IVec2;
    for i32, 3: unsafe impl ScalarOrVector for glam::IVec3;
    for i32, 4: unsafe impl ScalarOrVector for glam::IVec4;
}
