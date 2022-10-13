// build-fail

use spirv_std as _;

pub struct Boolthing {
    x: u32,
    y: u32,
    b: bool,
}

#[rust_gpu::spirv(fragment)]
pub fn fragment(
    input: bool,
    output: &mut bool,
    #[rust_gpu::spirv(push_constant)] push: &bool,
    #[rust_gpu::spirv(uniform)] uniform: &Boolthing,
) {
}
