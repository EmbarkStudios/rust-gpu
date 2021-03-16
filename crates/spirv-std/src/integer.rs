/// Abstract trait representing a SPIR-V integer type.
pub unsafe trait Integer: num_traits::PrimInt + crate::scalar::Scalar {}

unsafe impl Integer for u8 {}
unsafe impl Integer for u16 {}
unsafe impl Integer for u32 {}
unsafe impl Integer for u64 {}
unsafe impl Integer for i8 {}
unsafe impl Integer for i16 {}
unsafe impl Integer for i32 {}
unsafe impl Integer for i64 {}
