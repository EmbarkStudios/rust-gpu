/// Abstract trait representing a SPIR-V vector type.
pub unsafe trait Vector<T: crate::scalar::Scalar, const N: usize>: Default {}

#[cfg(feature = "glam")]
unsafe impl Vector<bool, 2> for glam::BVec2 {}
#[cfg(feature = "glam")]
unsafe impl Vector<bool, 3> for glam::BVec3 {}
#[cfg(feature = "glam")]
unsafe impl Vector<bool, 4> for glam::BVec4 {}

#[cfg(feature = "glam")]
unsafe impl Vector<f32, 2> for glam::Vec2 {}
#[cfg(feature = "glam")]
unsafe impl Vector<f32, 3> for glam::Vec3 {}
#[cfg(feature = "glam")]
unsafe impl Vector<f32, 3> for glam::Vec3A {}
#[cfg(feature = "glam")]
unsafe impl Vector<f32, 4> for glam::Vec4 {}

#[cfg(feature = "glam")]
unsafe impl Vector<f64, 2> for glam::DVec2 {}
#[cfg(feature = "glam")]
unsafe impl Vector<f64, 3> for glam::DVec3 {}
#[cfg(feature = "glam")]
unsafe impl Vector<f64, 4> for glam::DVec4 {}

#[cfg(feature = "glam")]
unsafe impl Vector<u32, 2> for glam::UVec2 {}
#[cfg(feature = "glam")]
unsafe impl Vector<u32, 3> for glam::UVec3 {}
#[cfg(feature = "glam")]
unsafe impl Vector<u32, 4> for glam::UVec4 {}

#[cfg(feature = "glam")]
unsafe impl Vector<i32, 2> for glam::IVec2 {}
#[cfg(feature = "glam")]
unsafe impl Vector<i32, 3> for glam::IVec3 {}
#[cfg(feature = "glam")]
unsafe impl Vector<i32, 4> for glam::IVec4 {}
