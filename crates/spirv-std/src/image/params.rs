use super::{Arrayed, Dimensionality, ImageFormat};
use crate::{integer::Integer, scalar::Scalar, vector::Vector};

/// Marker trait for arguments that accept single scalar values or vectors
/// of scalars. Defines 2-, 3- and 4-component vector types based on the sample type.
pub trait SampleType<const FORMAT: u32>: Scalar {
    /// A 2-component vector of this sample type
    type Vec2: Default;

    /// A 3-component vector of this sample type
    type Vec3: Default;

    /// A 4-component vector of this sample type
    type Vec4: Default;
}

/// Helper macro to implement `SampleType` of various formats for various scalar types.
macro_rules! sample_type_impls {
    ($($fmt:ident : $s:ty => ($v2:ty, $v3:ty, $v4:ty)),+ $(,)?) => {
        $(
            impl SampleType<{ ImageFormat::$fmt as u32 }> for $s {
                type Vec2 = $v2;
                type Vec3 = $v3;
                type Vec4 = $v4;
            }
        )+
    }
}

sample_type_impls! {
    Unknown: i8 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: i16 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: i64 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: u8 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: u16 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: u64 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Unknown: f64 => (glam::DVec2, glam::DVec3, glam::DVec4),
    Rgba32f: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba16f: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    R32f: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba8: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba8Snorm: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rg32f: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rg16f: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    R11fG11fB10f: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    R16f: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba16: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rgb10A2: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rg16: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rg8: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    R16: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    R8: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba16Snorm: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rg16Snorm: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rg8Snorm: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    R16Snorm: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    R8Snorm: f32 => (glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba32i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Rgba16i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Rgba8i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    R32i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Rg32i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Rg16i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Rg8i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    R16i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    R8i: i32 => (glam::IVec2, glam::IVec3, glam::IVec4),
    Rgba32ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Rgba16ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Rgba8ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    R32ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Rgb10A2ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Rg32ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Rg16ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    Rg8ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    R16ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    R8ui: u32 => (glam::UVec2, glam::UVec3, glam::UVec4),
    R64ui: u64 => (glam::UVec2, glam::UVec3, glam::UVec4),
    R64i: i64 => (glam::IVec2, glam::IVec3, glam::IVec4),
}

/// Marker trait for arguments that accept a coordinate for an [`crate::Image`].
pub trait ImageCoordinate<T, const DIM: u32, const ARRAYED: u32> {}

impl<S: Scalar> ImageCoordinate<S, { Dimensionality::OneD as u32 }, { Arrayed::False as u32 }>
    for S
{
}
impl<S: Scalar> ImageCoordinate<S, { Dimensionality::Buffer as u32 }, { Arrayed::False as u32 }>
    for S
{
}

impl<V: Vector<S, 2>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::TwoD as u32 }, { Arrayed::False as u32 }> for V
{
}
impl<V: Vector<S, 2>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::Rect as u32 }, { Arrayed::False as u32 }> for V
{
}
impl<V: Vector<S, 3>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::Cube as u32 }, { Arrayed::False as u32 }> for V
{
}
impl<V: Vector<S, 3>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::ThreeD as u32 }, { Arrayed::False as u32 }> for V
{
}

impl<V: Vector<S, 3>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::TwoD as u32 }, { Arrayed::True as u32 }> for V
{
}
impl<V: Vector<S, 3>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::Rect as u32 }, { Arrayed::True as u32 }> for V
{
}
impl<V: Vector<S, 4>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::Cube as u32 }, { Arrayed::True as u32 }> for V
{
}
impl<V: Vector<S, 4>, S: Scalar>
    ImageCoordinate<S, { Dimensionality::ThreeD as u32 }, { Arrayed::True as u32 }> for V
{
}

/// Marker trait for arguments that are valid for a [`crate::image::Dimensionality::SubpassData`] image query.
pub trait ImageCoordinateSubpassData<T, const ARRAYED: u32> {}
impl<V: Vector<I, 2>, I: Integer> ImageCoordinateSubpassData<I, { Arrayed::False as u32 }> for V {}
impl<V: Vector<I, 3>, I: Integer> ImageCoordinateSubpassData<I, { Arrayed::True as u32 }> for V {}
