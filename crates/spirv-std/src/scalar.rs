//! Traits related to scalars.

/// Abstract trait representing a SPIR-V scalar type.
///
/// # Safety
/// Implementing this trait on non-scalar types breaks assumptions of other unsafe code, and should
/// not be done.
pub unsafe trait Scalar: Copy + Default + crate::sealed::Sealed {}

/// Abstract trait representing a SPIR-V scalar type. It may also be a vector type.
///
/// # Safety
/// Implementing this trait on non-scalar types breaks assumptions of other unsafe code, and should
/// not be done.
pub unsafe trait ScalarVector: Copy + Default {
    /// Dimension of the vector, 1 if it is a scalar
    const DIM: usize;
}

macro_rules! impl_scalars {
    (unsafe impl Scalar for $($typ:ty)*;) => {
        $(
            unsafe impl Scalar for $typ {}
            unsafe impl ScalarVector for $typ {
                const DIM: usize = 1;
            }
        )*
    };

    (unsafe impl ScalarVector, $num: literal for $($typ:ty)*;) => {
        $(
            unsafe impl ScalarVector for $typ {
                const DIM: usize = $num;
            }
        )*
    };
}

impl_scalars! {
    unsafe impl Scalar for bool f32 f64 u8 u16 u32 u64 i8 i16 i32 i64;
}

impl_scalars! {
    unsafe impl ScalarVector, 2 for glam::BVec2 glam::Vec2 glam::DVec2 glam::UVec2 glam::IVec2;
}

impl_scalars! {
    unsafe impl ScalarVector, 3 for glam::BVec3 glam::Vec3 glam::DVec3 glam::UVec3 glam::IVec3;
}

impl_scalars! {
    unsafe impl ScalarVector, 4 for glam::BVec4 glam::Vec4 glam::DVec4 glam::UVec4 glam::IVec4;
}
