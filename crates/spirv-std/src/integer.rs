/// Abstract trait representing any SPIR-V integer type.
pub unsafe trait Integer: num_traits::PrimInt + crate::scalar::Scalar {
    const WIDTH: usize;
    const SIGNED: bool;
}

/// A trait for being generic over signed integer types.
pub trait SignedInteger: Integer {}
/// A trait for being generic over unsigned integer types.
pub trait UnsignedInteger: Integer {}

macro_rules! impl_numbers {
    (impl UnsignedInteger for $typ:ty;) => {
        unsafe impl Integer for $typ {
            const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
            const SIGNED: bool = false;
        }

        impl UnsignedInteger for $typ {}
    };
    (impl SignedInteger for $typ:ty;) => {
        unsafe impl Integer for $typ {
            const WIDTH: usize = core::mem::size_of::<$typ>() * 8;
            const SIGNED: bool = true;
        }

        impl SignedInteger for $typ {}
    };
    ($(impl $trait:ident for $typ:ty;)+) => {
        $(impl_numbers!(impl $trait for $typ;);)+
    };

}

impl_numbers! {
    impl UnsignedInteger for u8;
    impl UnsignedInteger for u16;
    impl UnsignedInteger for u32;
    impl UnsignedInteger for u64;
    impl SignedInteger for i8;
    impl SignedInteger for i16;
    impl SignedInteger for i32;
    impl SignedInteger for i64;
}
