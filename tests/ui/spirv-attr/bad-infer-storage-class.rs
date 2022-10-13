// Tests that storage class inference fails correctly
// build-fail

use spirv_std::Image;

#[rust_gpu::spirv(vertex)]
pub fn main(
    #[rust_gpu::spirv(uniform)] error: &Image!(2D, type=f32),
    #[rust_gpu::spirv(uniform_constant)] warning: &Image!(2D, type=f32),
) {
}

// https://github.com/EmbarkStudios/rust-gpu/issues/585
#[rust_gpu::spirv(vertex)]
pub fn issue_585(invalid: Image!(2D, type=f32)) {}
