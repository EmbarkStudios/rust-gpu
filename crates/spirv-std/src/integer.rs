//! Traits related to integers.

/// Abstract trait representing any SPIR-V integer type.
///
/// # Safety
/// Implementing this trait on non-primitive-integer types breaks assumptions of other unsafe code,
/// and should not be done.
pub unsafe trait Integer: num_traits::PrimInt + crate::scalar::Scalar {
    /// Width of the integer, in bits.
    const WIDTH: usize;
    /// If the integer is signed: true means signed, false means unsigned.
    const SIGNED: bool;
}

/// Abstract trait representing any SPIR-V integer type. It may also be a vector type.
///
/// # Safety
/// Implementing this trait on non-primitive-integer types breaks assumptions of other unsafe code,
/// and should not be done.
pub unsafe trait IntegerVector: crate::scalar::ScalarVector {
    /// Width of the integer, in bits.
    const WIDTH: usize;
    /// If the integer is signed: true means signed, false means unsigned.
    const SIGNED: bool;
}

/// A trait for being generic over signed integer types.
pub trait SignedInteger: Integer {}
/// A trait for being generic over signed integer types. It may also be a vector type.
pub trait SignedIntegerVector: IntegerVector {}

/// A trait for being generic over unsigned integer types.
pub trait UnsignedInteger: Integer {}
/// A trait for being generic over unsigned integer types. It may also be a vector type.
pub trait UnsignedIntegerVector: IntegerVector {}

macro_rules! impl_numbers {
    (impl UnsignedInteger for $($typ:ty)*;) => {
        $(
            unsafe impl Integer for $typ {
                const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
                const SIGNED: bool = false;
            }

            unsafe impl IntegerVector for $typ {
                const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
                const SIGNED: bool = false;
            }

            impl UnsignedInteger for $typ {}
            impl UnsignedIntegerVector for $typ {}
        )*
    };

    (impl UnsignedIntegerVector for $($typ:ty)*;) => {
        $(
            unsafe impl IntegerVector for $typ {
                const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
                const SIGNED: bool = false;
            }

            impl UnsignedIntegerVector for $typ {}
        )*
    };

    (impl SignedInteger for $($typ:ty)*;) => {
        $(
            unsafe impl Integer for $typ {
                const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
                const SIGNED: bool = true;
            }

            unsafe impl IntegerVector for $typ {
                const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
                const SIGNED: bool = true;
            }

            impl SignedInteger for $typ {}
            impl SignedIntegerVector for $typ {}
        )*
    };

    (impl SignedIntegerVector for $($typ:ty)*;) => {
        $(
            unsafe impl IntegerVector for $typ {
                const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
                const SIGNED: bool = true;
            }

            impl SignedIntegerVector for $typ {}
        )*
    };

    ($(impl $trait:ident for $($typ:ty)*;)+) => {
        $(impl_numbers!(impl $trait for $($typ)*;);)+
    };
}

impl_numbers! {
    impl UnsignedInteger for u8 u16 u32 u64;
    impl UnsignedIntegerVector for glam::UVec2 glam::UVec3 glam::UVec4;
    impl SignedInteger for i8 i16 i32 i64;
    impl SignedIntegerVector for glam::IVec2 glam::IVec3 glam::IVec4;
}
