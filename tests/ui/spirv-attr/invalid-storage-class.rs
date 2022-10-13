// Tests that certain storage class `#[rust_gpu::spirv(...)]` attributes are disallowed.

// build-fail

use spirv_std as _;

#[rust_gpu::spirv(vertex)]
fn _entry(
    #[rust_gpu::spirv(input)] _: (),
    #[rust_gpu::spirv(output)] _: (),
    #[rust_gpu::spirv(private)] _: (),
    #[rust_gpu::spirv(function)] _: (),
    #[rust_gpu::spirv(generic)] _: (),
) {
}
