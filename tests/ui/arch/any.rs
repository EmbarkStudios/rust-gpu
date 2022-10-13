// build-pass

#[rust_gpu::spirv(fragment)]
pub fn main() {
    let vector = glam::BVec2::new(true, false);
    assert!(spirv_std::arch::any(vector));
}
