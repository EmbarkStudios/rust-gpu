/// Abstract trait representing a SPIR-V vector type.
#[cfg(feature = "const-generics")]
pub unsafe trait Vector<T: crate::scalar::Scalar, const N: usize>: Default {}
