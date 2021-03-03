/// Abstract trait representing a SPIR-V vector type.
pub trait Vector<T: crate::scalar::Scalar>: crate::sealed::Sealed + Default {}

impl Vector<bool> for glam::BVec2 {}
impl Vector<bool> for glam::BVec3 {}
impl Vector<bool> for glam::BVec4 {}
impl Vector<f32> for glam::Vec2 {}
impl Vector<f32> for glam::Vec3 {}
impl Vector<f32> for glam::Vec3A {}
impl Vector<f32> for glam::Vec4 {}
impl Vector<u32> for glam::UVec2 {}
impl Vector<u32> for glam::UVec3 {}
impl Vector<u32> for glam::UVec4 {}
impl Vector<i32> for glam::IVec2 {}
impl Vector<i32> for glam::IVec3 {}
impl Vector<i32> for glam::IVec4 {}
