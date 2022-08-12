//! Traits and helper functions related to numbers.

/// Abstract trait representing a SPIR-V integer or floating-point type.
pub trait Number: crate::scalar::Scalar {}

impl Number for u8 {}
impl Number for u16 {}
impl Number for u32 {}
impl Number for u64 {}
impl Number for i8 {}
impl Number for i16 {}
impl Number for i32 {}
impl Number for i64 {}
impl Number for f32 {}
impl Number for f64 {}
