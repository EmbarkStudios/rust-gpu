// Tests that using `&mut` (or interior mutability) with read-only storage classes
// does actually error (see `mutability-errors.stderr` for the error messages).
// build-fail

use core::sync::atomic::AtomicU32;
use spirv_std::{image::Image2d, spirv};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] implicit_uniform_constant_mut: &mut Image2d,
    #[spirv(uniform_constant, descriptor_set = 0, binding = 0)] uniform_constant_mut: &mut Image2d,
    #[spirv(uniform, descriptor_set = 0, binding = 0)] uniform_mut: &mut u32,
    #[spirv(uniform, descriptor_set = 0, binding = 0)] uniform_interior_mut: &AtomicU32,
    #[spirv(push_constant)] push_constant_mut: &mut u32,
    #[spirv(push_constant)] push_constant_interior_mut: &AtomicU32,
) {
}
