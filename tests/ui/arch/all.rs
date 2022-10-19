use spirv_std::spirv;

// build-pass

#[spirv(fragment)]
pub fn main() {
    let vector = glam::BVec2::new(true, true);
    assert!(spirv_std::arch::all(vector));
}
