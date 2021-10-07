use super::{Arrayed, Dimensionality, ImageFormat};
use crate::{integer::Integer, scalar::Scalar, vector::Vector};

/// Marker trait for arguments that accept single scalar values or vectors
/// of scalars.
pub trait SampleType<const FORMAT: u32>: Scalar {}

impl SampleType<{ ImageFormat::Unknown as u32 }> for i8 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for i16 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for i32 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for i64 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for u8 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for u16 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for u32 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for u64 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Unknown as u32 }> for f64 {}
impl SampleType<{ ImageFormat::Rgba32f as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba16f as u32 }> for f32 {}
impl SampleType<{ ImageFormat::R32f as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba8 as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba8Snorm as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rg32f as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rg16f as u32 }> for f32 {}
impl SampleType<{ ImageFormat::R11fG11fB10f as u32 }> for f32 {}
impl SampleType<{ ImageFormat::R16f as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba16 as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rgb10A2 as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rg16 as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rg8 as u32 }> for f32 {}
impl SampleType<{ ImageFormat::R16 as u32 }> for f32 {}
impl SampleType<{ ImageFormat::R8 as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba16Snorm as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rg16Snorm as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rg8Snorm as u32 }> for f32 {}
impl SampleType<{ ImageFormat::R16Snorm as u32 }> for f32 {}
impl SampleType<{ ImageFormat::R8Snorm as u32 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba32i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::Rgba16i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::Rgba8i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::R32i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::Rg32i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::Rg16i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::Rg8i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::R16i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::R8i as u32 }> for i32 {}
impl SampleType<{ ImageFormat::Rgba32ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::Rgba16ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::Rgba8ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::R32ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::Rgb10A2ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::Rg32ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::Rg16ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::Rg8ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::R16ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::R8ui as u32 }> for u32 {}
impl SampleType<{ ImageFormat::R64ui as u32 }> for u64 {}
impl SampleType<{ ImageFormat::R64i as u32 }> for i64 {}

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
