/// Abstract trait representing a SPIR-V integer type.
pub trait Integer: num_traits::PrimInt + crate::scalar::Scalar {}

impl Integer for u8 {}
impl Integer for u16 {}
impl Integer for u32 {}
impl Integer for u64 {}
impl Integer for i8 {}
impl Integer for i16 {}
impl Integer for i32 {}
impl Integer for i64 {}
