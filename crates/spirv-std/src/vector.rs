//! Traits related to vectors.

use glam::{Vec3Swizzles, Vec4Swizzles};

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

/// Trait that implements slicing of a vector into a scalar or vector of lower dimensions, by
/// ignoring the highter dimensions
pub trait VectorTruncateInto<T> {
    /// Slices the vector into a lower dimensional type by ignoring the higher components
    fn truncate_into(self) -> T;
}

macro_rules! vec_trunc_impl {
    ($a:ty, $b:ty, $self:ident $(.$($e:tt)*)?) => {
        impl VectorTruncateInto<$a> for $b {
            fn truncate_into($self) -> $a {
                $self $(. $($e)*)?
            }
        }
    };
}
macro_rules! vec_trunc_impls {
    ($s:ty, $v2:ty, $v3:ty, $v4:ty) => {
        vec_trunc_impl! {$s, $s, self}
        vec_trunc_impl! {$s, $v2, self.x}
        vec_trunc_impl! {$s, $v3, self.x}
        vec_trunc_impl! {$s, $v4, self.x}

        vec_trunc_impl! {$v2, $v2, self}
        vec_trunc_impl! {$v2, $v3, self.xy()}
        vec_trunc_impl! {$v2, $v4, self.xy()}

        vec_trunc_impl! {$v3, $v3, self}
        vec_trunc_impl! {$v3, $v4, self.xyz()}

        vec_trunc_impl! {$v4, $v4, self}
    };
}

vec_trunc_impls! { f32, glam::Vec2, glam::Vec3, glam::Vec4 }
vec_trunc_impls! { f64, glam::DVec2, glam::DVec3, glam::DVec4 }
vec_trunc_impls! { i32, glam::IVec2, glam::IVec3, glam::IVec4 }
vec_trunc_impls! { u32, glam::UVec2, glam::UVec3, glam::UVec4 }
