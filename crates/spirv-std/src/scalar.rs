//! Traits related to scalars.

/// Abstract trait representing either a vector or a scalar type.
///
/// # Safety
/// Implementing this trait on non-scalar or non-vector types may break assumptions about other
/// unsafe code, and should not be done.
pub unsafe trait VectorOrScalar: Default {
    /// Either the scalar component type of the vector or the scalar itself.
    type Scalar: Scalar;
}

unsafe impl VectorOrScalar for bool {
    type Scalar = bool;
}
unsafe impl VectorOrScalar for f32 {
    type Scalar = f32;
}
unsafe impl VectorOrScalar for f64 {
    type Scalar = f64;
}
unsafe impl VectorOrScalar for u8 {
    type Scalar = u8;
}
unsafe impl VectorOrScalar for u16 {
    type Scalar = u16;
}
unsafe impl VectorOrScalar for u32 {
    type Scalar = u32;
}
unsafe impl VectorOrScalar for u64 {
    type Scalar = u64;
}
unsafe impl VectorOrScalar for i8 {
    type Scalar = i8;
}
unsafe impl VectorOrScalar for i16 {
    type Scalar = i16;
}
unsafe impl VectorOrScalar for i32 {
    type Scalar = i32;
}
unsafe impl VectorOrScalar for i64 {
    type Scalar = i64;
}

/// Abstract trait representing a SPIR-V scalar type.
///
/// # Safety
/// Implementing this trait on non-scalar types breaks assumptions of other unsafe code, and should
/// not be done.
pub unsafe trait Scalar:
    VectorOrScalar<Scalar = Self> + Copy + Default + crate::sealed::Sealed
{
}

unsafe impl Scalar for bool {}
unsafe impl Scalar for f32 {}
unsafe impl Scalar for f64 {}
unsafe impl Scalar for u8 {}
unsafe impl Scalar for u16 {}
unsafe impl Scalar for u32 {}
unsafe impl Scalar for u64 {}
unsafe impl Scalar for i8 {}
unsafe impl Scalar for i16 {}
unsafe impl Scalar for i32 {}
unsafe impl Scalar for i64 {}
