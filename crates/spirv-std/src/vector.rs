/// Abstract trait representing a SPIR-V vector type.
pub unsafe trait Vector<T: crate::scalar::Scalar, const N: usize>: Default {}
