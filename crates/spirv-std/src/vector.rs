/// Abstract trait representing a SPIR-V vector type.
pub trait Vector<T: crate::scalar::Scalar, const N: usize>:
    crate::sealed::Sealed + Default
{
}

impl Vector<bool, 2> for glam::BVec2 {}
impl Vector<bool, 3> for glam::BVec3 {}
impl Vector<bool, 4> for glam::BVec4 {}
impl Vector<f32, 2> for glam::Vec2 {}
impl Vector<f32, 3> for glam::Vec3 {}
impl Vector<f32, 3> for glam::Vec3A {}
impl Vector<f32, 4> for glam::Vec4 {}
impl Vector<u32, 2> for glam::UVec2 {}
impl Vector<u32, 3> for glam::UVec3 {}
impl Vector<u32, 4> for glam::UVec4 {}
impl Vector<i32, 2> for glam::IVec2 {}
impl Vector<i32, 3> for glam::IVec3 {}
impl Vector<i32, 4> for glam::IVec4 {}
