// build-pass

#[spirv(fragment)]
pub fn main() {
    let operand: i32 = -5;
    let vector = glam::IVec2::new(-5, -0);
    assert!(spirv_std::arch::s_negate_vector(vector) == glam::IVec2::new(5, 0));
}
