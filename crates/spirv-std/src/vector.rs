//! Traits related to vectors.

/// Abstract trait representing a SPIR-V vector type.
///
/// # Safety
/// Implementing this trait on non-simd-vector types breaks assumptions of other unsafe code, and
/// should not be done.
pub unsafe trait Vector<T: crate::scalar::Scalar, const N: usize>: Default {
    /// Refers to a helper type that implements all kinds of references to concrete vectors
    /// within the same library
    type VectorTypeLib: VectorTypeRef<T, N>;
}

/// Trait that refers to a concrete vector type with specific generic parameters, to aid generic
/// functions in finding concrete vector types of different generic parameters implemented by
/// the same library.
///
/// A type that implements such a trait, is expected to implement many concrete variants of this
/// trait. Such a type is said to be a 'vector type library'.
///
/// For example, if a generic function has as input `T impl Vector<i3,2>` and it
/// wants to return a `Vector<f32,3>` of the same library of implementations, it can find it the
/// concrete vector type by doing:
///     `<T::VectorTypeLib as VectorTypeRef<f32,3>>::Vector`
/// Alternatively, using the `VectorFromVector!` macro:
///     `VectorFromVector!(T, f32, 3)`
pub trait VectorTypeRef<T: crate::scalar::Scalar, const N: usize> {
    /// Concrete type that implements the vector type with these generic parameters
    type Vector: Vector<T, N, VectorTypeLib = Self>;
}

/// Helper macro that allows you to find a vector type with certain generic parameters
/// from the same 'vector type library' as another vector type.
#[macro_export]
macro_rules! VectorFromVector {
    ($vec: ty, $comp: ty, $dim: expr) => {
        <<$vec>::VectorTypeLib as VectorTypeRef<$comp, $dim>>::Vector
    };
}

/// Glam vector type library. It implements all relevant VectorTypeRef<T,N>s
pub struct GlamVectorTypeLib;

#[cfg(feature = "glam")]
unsafe impl Vector<f32, 2> for glam::Vec2 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<f32, 3> for glam::Vec3 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<f32, 3> for glam::Vec3A {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<f32, 4> for glam::Vec4 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<f32, 2> for GlamVectorTypeLib {
    type Vector = glam::Vec2;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<f32, 3> for GlamVectorTypeLib {
    type Vector = glam::Vec3;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<f32, 4> for GlamVectorTypeLib {
    type Vector = glam::Vec4;
}

#[cfg(feature = "glam")]
unsafe impl Vector<f64, 2> for glam::DVec2 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<f64, 3> for glam::DVec3 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<f64, 4> for glam::DVec4 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<f64, 2> for GlamVectorTypeLib {
    type Vector = glam::DVec2;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<f64, 3> for GlamVectorTypeLib {
    type Vector = glam::DVec3;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<f64, 4> for GlamVectorTypeLib {
    type Vector = glam::DVec4;
}

#[cfg(feature = "glam")]
unsafe impl Vector<u32, 2> for glam::UVec2 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<u32, 3> for glam::UVec3 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<u32, 4> for glam::UVec4 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<u32, 2> for GlamVectorTypeLib {
    type Vector = glam::UVec2;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<u32, 3> for GlamVectorTypeLib {
    type Vector = glam::UVec3;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<u32, 4> for GlamVectorTypeLib {
    type Vector = glam::UVec4;
}

#[cfg(feature = "glam")]
unsafe impl Vector<i32, 2> for glam::IVec2 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<i32, 3> for glam::IVec3 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
unsafe impl Vector<i32, 4> for glam::IVec4 {
    type VectorTypeLib = GlamVectorTypeLib;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<i32, 2> for GlamVectorTypeLib {
    type Vector = glam::IVec2;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<i32, 3> for GlamVectorTypeLib {
    type Vector = glam::IVec3;
}
#[cfg(feature = "glam")]
impl VectorTypeRef<i32, 4> for GlamVectorTypeLib {
    type Vector = glam::IVec4;
}
