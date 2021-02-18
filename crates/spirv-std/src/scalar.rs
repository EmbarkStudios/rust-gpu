/// Abstract trait representing a SPIR-V scalar type.
pub trait Scalar: Copy + Default + crate::sealed::Sealed {}

impl Scalar for bool {}
impl Scalar for f32 {}
impl Scalar for u8 {}
impl Scalar for u16 {}
impl Scalar for u32 {}
impl Scalar for u64 {}
impl Scalar for i8 {}
impl Scalar for i16 {}
impl Scalar for i32 {}
impl Scalar for i64 {}
