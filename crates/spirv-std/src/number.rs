//! Traits and helper functions related to numbers.

/// Abstract trait representing a SPIR-V integer or floating-point type.
pub trait Number: crate::scalar::Scalar {}

/// Abstract trait representing a SPIR-V integer or floating-point type. It may also be a vector type.
pub trait NumberVector: crate::scalar::ScalarVector {}

macro_rules! impl_numbers {
    (unsafe impl Number for $($typ:ty)*;) => {
        $(
            impl Number for $typ {}
            impl NumberVector for $typ {}
        )*
    };

    (unsafe impl NumberVector for $($typ:ty)*;) => {
        $(
            impl NumberVector for $typ {}
        )*
    };

    ($(unsafe impl $trait:ident for $($typ:ty)*;)+) => {
        $(impl_numbers!(unsafe impl $trait for $($typ)*;);)+
    };
}

impl_numbers! {
    unsafe impl Number for u8 u16 u32 u64 i8 i16 i32 i64 f32 f64;
    unsafe impl NumberVector for glam::Vec2 glam::DVec2 glam::UVec2 glam::IVec2;
    unsafe impl NumberVector for glam::Vec3 glam::DVec3 glam::UVec3 glam::IVec3;
    unsafe impl NumberVector for glam::Vec4 glam::DVec4 glam::UVec4 glam::IVec4;
}
