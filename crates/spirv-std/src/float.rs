/// Abstract trait representing a SPIR-V floating point type.
pub unsafe trait Float: num_traits::Float + crate::scalar::Scalar + Default {
    const WIDTH: usize;
}

unsafe impl Float for f32 {
    const WIDTH: usize = 32;
}

unsafe impl Float for f64 {
    const WIDTH: usize = 64;
}
