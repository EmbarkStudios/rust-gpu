use super::{Arrayed, Dimensionality, ImageFormat};
use crate::{scalar::Scalar, vector::Vector};

/// Marker trait for arguments that accept single scalar values or vectors
/// of scalars.
pub trait SampleType<const FORMAT: ImageFormat>: Scalar {}

impl SampleType<{ ImageFormat::Unknown }> for i8 {}
impl SampleType<{ ImageFormat::Unknown }> for i16 {}
impl SampleType<{ ImageFormat::Unknown }> for i32 {}
impl SampleType<{ ImageFormat::Unknown }> for i64 {}
impl SampleType<{ ImageFormat::Unknown }> for u8 {}
impl SampleType<{ ImageFormat::Unknown }> for u16 {}
impl SampleType<{ ImageFormat::Unknown }> for u32 {}
impl SampleType<{ ImageFormat::Unknown }> for u64 {}
impl SampleType<{ ImageFormat::Unknown }> for f32 {}
impl SampleType<{ ImageFormat::Unknown }> for f64 {}
impl SampleType<{ ImageFormat::Rgba32f }> for f32 {}
impl SampleType<{ ImageFormat::Rgba16f }> for f32 {}
impl SampleType<{ ImageFormat::R32f }> for f32 {}
impl SampleType<{ ImageFormat::Rgba8 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba8Snorm }> for f32 {}
impl SampleType<{ ImageFormat::Rg32f }> for f32 {}
impl SampleType<{ ImageFormat::Rg16f }> for f32 {}
impl SampleType<{ ImageFormat::R11fG11fB10f }> for f32 {}
impl SampleType<{ ImageFormat::R16f }> for f32 {}
impl SampleType<{ ImageFormat::Rgba16 }> for f32 {}
impl SampleType<{ ImageFormat::Rgb10A2 }> for f32 {}
impl SampleType<{ ImageFormat::Rg16 }> for f32 {}
impl SampleType<{ ImageFormat::Rg8 }> for f32 {}
impl SampleType<{ ImageFormat::R16 }> for f32 {}
impl SampleType<{ ImageFormat::R8 }> for f32 {}
impl SampleType<{ ImageFormat::Rgba16Snorm }> for f32 {}
impl SampleType<{ ImageFormat::Rg16Snorm }> for f32 {}
impl SampleType<{ ImageFormat::Rg8Snorm }> for f32 {}
impl SampleType<{ ImageFormat::R16Snorm }> for f32 {}
impl SampleType<{ ImageFormat::R8Snorm }> for f32 {}
impl SampleType<{ ImageFormat::Rgba32i }> for i32 {}
impl SampleType<{ ImageFormat::Rgba16i }> for i32 {}
impl SampleType<{ ImageFormat::Rgba8i }> for i32 {}
impl SampleType<{ ImageFormat::R32i }> for i32 {}
impl SampleType<{ ImageFormat::Rg32i }> for i32 {}
impl SampleType<{ ImageFormat::Rg16i }> for i32 {}
impl SampleType<{ ImageFormat::Rg8i }> for i32 {}
impl SampleType<{ ImageFormat::R16i }> for i32 {}
impl SampleType<{ ImageFormat::R8i }> for i32 {}
impl SampleType<{ ImageFormat::Rgba32ui }> for u32 {}
impl SampleType<{ ImageFormat::Rgba16ui }> for u32 {}
impl SampleType<{ ImageFormat::Rgba8ui }> for u32 {}
impl SampleType<{ ImageFormat::R32ui }> for u32 {}
impl SampleType<{ ImageFormat::Rgb10A2ui }> for u32 {}
impl SampleType<{ ImageFormat::Rg32ui }> for u32 {}
impl SampleType<{ ImageFormat::Rg16ui }> for u32 {}
impl SampleType<{ ImageFormat::Rg8ui }> for u32 {}
impl SampleType<{ ImageFormat::R16ui }> for u32 {}
impl SampleType<{ ImageFormat::R8ui }> for u32 {}
impl SampleType<{ ImageFormat::R64ui }> for u64 {}
impl SampleType<{ ImageFormat::R64i }> for i64 {}

/// Marker trait for arguments that accept a coordinate for an [`crate::Image`].
pub trait ImageCoordinate<T, const DIM: Dimensionality, const ARRAYED: Arrayed> {}

impl<S: Scalar> ImageCoordinate<S, { Dimensionality::OneD }, { Arrayed::False }> for S {}
impl<S: Scalar> ImageCoordinate<S, { Dimensionality::Buffer }, { Arrayed::False }> for S {}

impl<V: Vector<S, 2>, S: Scalar> ImageCoordinate<S, { Dimensionality::TwoD }, { Arrayed::False }> for V {}
impl<V: Vector<S, 2>, S: Scalar> ImageCoordinate<S, { Dimensionality::Rect }, { Arrayed::False }> for V {}
impl<V: Vector<S, 3>, S: Scalar> ImageCoordinate<S, { Dimensionality::Cube }, { Arrayed::False }> for V {}
impl<V: Vector<S, 3>, S: Scalar> ImageCoordinate<S, { Dimensionality::ThreeD }, { Arrayed::False }> for V {}

impl<V: Vector<S, 3>, S: Scalar> ImageCoordinate<S, { Dimensionality::TwoD }, { Arrayed::True }> for V {}
impl<V: Vector<S, 3>, S: Scalar> ImageCoordinate<S, { Dimensionality::Rect }, { Arrayed::True }> for V {}
impl<V: Vector<S, 4>, S: Scalar> ImageCoordinate<S, { Dimensionality::Cube }, { Arrayed::True }> for V {}
impl<V: Vector<S, 4>, S: Scalar> ImageCoordinate<S, { Dimensionality::ThreeD }, { Arrayed::True }> for V {}
