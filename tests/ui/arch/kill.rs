// build-pass

#[rust_gpu::spirv(fragment)]
pub fn main() {
    spirv_std::arch::kill();
}
