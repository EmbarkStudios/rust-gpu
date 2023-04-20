use super::{Arrayed, Dimensionality, ImageFormat};
use crate::{integer::Integer, scalar::Scalar, vector::Vector, vector::VectorTruncateInto};

/// Marker trait for arguments that accept single scalar values or vectors
/// of scalars. Defines 2-, 3- and 4-component vector types based on the sample type.
pub trait SampleType<const FORMAT: u32, const COMPONENTS: u32>: Scalar {
    /// The default vector/scalar of ths sample type
    type SampleResult: Default;

    /// A 2-component vector of this sample type
    type Vec2: Default;

    /// A 3-component vector of this sample type
    type Vec3: Default;

    /// A 4-component vector of this sample type
    type Vec4: Default + VectorTruncateInto<Self::SampleResult>;
}

/// Helper macro to implement `SampleType` of various formats for various scalar types.
macro_rules! sample_type_impls {
    ($($fmt:ident : $n:tt*$s:ty => ($v1:ty, $v2:ty, $v3:ty, $v4:ty)),+ $(,)?) => {
        $(sample_type_impls!{@single_rule, $fmt : $n*$s => ($v1,$v2,$v3,$v4)})+
    };
    (@single_rule, $fmt:ident : n*$s:ty => ($v1:ty, $v2:ty, $v3:ty, $v4:ty)) => {
        impl SampleType<{ ImageFormat::$fmt as u32 }, 1> for $s {
            type SampleResult = $v1;
            type Vec2 = $v2;
            type Vec3 = $v3;
            type Vec4 = $v4;
        }
        impl SampleType<{ ImageFormat::$fmt as u32 }, 2> for $s {
            type SampleResult = $v2;
            type Vec2 = $v2;
            type Vec3 = $v3;
            type Vec4 = $v4;
        }
        impl SampleType<{ ImageFormat::$fmt as u32 }, 3> for $s {
            type SampleResult = $v3;
            type Vec2 = $v2;
            type Vec3 = $v3;
            type Vec4 = $v4;
        }
        impl SampleType<{ ImageFormat::$fmt as u32 }, 4> for $s {
            type SampleResult = $v4;
            type Vec2 = $v2;
            type Vec3 = $v3;
            type Vec4 = $v4;
        }
    };
    (@single_rule, $($fmt:ident : 1*$s:ty => ($v1:ty, $v2:ty, $v3:ty, $v4:ty)),+ $(,)?) => {
        $(
            impl SampleType<{ ImageFormat::$fmt as u32 }, 1> for $s {
                type SampleResult = $v1;
                type Vec2 = $v2;
                type Vec3 = $v3;
                type Vec4 = $v4;
            }
        )+
    };
    (@single_rule, $($fmt:ident : 2*$s:ty => ($v1:ty, $v2:ty, $v3:ty, $v4:ty)),+ $(,)?) => {
        $(
            impl SampleType<{ ImageFormat::$fmt as u32 }, 2> for $s {
                type SampleResult = $v2;
                type Vec2 = $v2;
                type Vec3 = $v3;
                type Vec4 = $v4;
            }
        )+
    };
    (@single_rule, $($fmt:ident : 3*$s:ty => ($v1:ty, $v2:ty, $v3:ty, $v4:ty)),+ $(,)?) => {
        $(
            impl SampleType<{ ImageFormat::$fmt as u32 }, 3> for $s {
                type SampleResult = $v3;
                type Vec2 = $v2;
                type Vec3 = $v3;
                type Vec4 = $v4;
            }
        )+
    };
    (@single_rule, $($fmt:ident : 4*$s:ty => ($v1:ty, $v2:ty, $v3:ty, $v4:ty)),+ $(,)?) => {
        $(
            impl SampleType<{ ImageFormat::$fmt as u32 }, 4> for $s {
                type SampleResult = $v4;
                type Vec2 = $v2;
                type Vec3 = $v3;
                type Vec4 = $v4;
            }
        )+
    };
}

sample_type_impls! {
    Unknown: n*i8 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: n*i16 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: n*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: n*i64 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Unknown: n*u8 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: n*u16 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: n*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: n*u64 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Unknown: n*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Unknown: n*f64 => (f64, glam::DVec2, glam::DVec3, glam::DVec4),
    Rgba32f: 4*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba16f: 4*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    R32f: 1*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba8: 4*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba8Snorm: 4*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rg32f: 2*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rg16f: 2*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    R11fG11fB10f: 3*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    R16f: 1*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba16: 4*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rgb10A2: 4*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rg16: 2*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rg8: 2*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    R16: 1*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    R8: 1*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba16Snorm: 4*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rg16Snorm: 2*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rg8Snorm: 2*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    R16Snorm: 1*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    R8Snorm: 1*f32 => (f32, glam::Vec2, glam::Vec3, glam::Vec4),
    Rgba32i: 4*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Rgba16i: 4*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Rgba8i: 4*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    R32i: 1*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Rg32i: 2*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Rg16i: 2*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Rg8i: 2*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    R16i: 1*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    R8i: 1*i32 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
    Rgba32ui: 4*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Rgba16ui: 4*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Rgba8ui: 4*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    R32ui: 1*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Rgb10A2ui: 4*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Rg32ui: 2*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Rg16ui: 2*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    Rg8ui: 2*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    R16ui: 1*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    R8ui: 1*u32 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    R64ui: 1*u64 => (u32, glam::UVec2, glam::UVec3, glam::UVec4),
    R64i: 1*i64 => (i32, glam::IVec2, glam::IVec3, glam::IVec4),
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
