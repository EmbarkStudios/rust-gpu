//! Traits related to vectors.

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
