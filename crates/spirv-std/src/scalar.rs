/// Abstract trait representing a SPIR-V scalar type.
pub unsafe trait Scalar: Copy + Default + crate::sealed::Sealed {}

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
