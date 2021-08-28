/// Abstract trait representing a SPIR-V matrix type.

/// All types implement this trait is expressed as `OpTypeMatrix` in SPIR-V.
pub unsafe trait Matrix<T: crate::scalar::Scalar, const M: usize, const N: usize>:
    Default
{
}

#[derive(Default)]
struct DummyMatrix;

// FIXME: To get DefId of Matrix trait, we need at least one implementation.
// Because we're using `TyCtxt::all_trait_implementations` to get Matrix trait.
unsafe impl Matrix<f32, 0, 0> for DummyMatrix {}

/*
/// TODO
/// To work glam matrix as SPIR-V matrix, it's also needed to impl Matrix for its inner types
/// e.g. glam::core::storage::Column3<T> but it's private

#[cfg(feature = "glam")]
unsafe impl Matrix<f32, 2, 2> for glam::Mat2 {}

#[cfg(feature = "glam")]
unsafe impl Matrix<f32, 3, 3> for glam::Mat3 {}

#[cfg(feature = "glam")]
unsafe impl Matrix<f32, 4, 4> for glam::Mat4 {}

#[cfg(feature = "glam")]
unsafe impl Matrix<f64, 2, 2> for glam::DMat2 {}

#[cfg(feature = "glam")]
unsafe impl Matrix<f64, 3, 3> for glam::DMat3 {}

#[cfg(feature = "glam")]
unsafe impl Matrix<f64, 4, 4> for glam::DMat4 {}
*/
