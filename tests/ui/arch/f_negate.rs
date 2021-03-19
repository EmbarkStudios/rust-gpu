// build-pass

#[spirv(fragment)]
pub fn main() {
    let operand: f32 = -5.0;
    let vector = glam::Vec2::new(-5.0, -0.0);
    assert!(spirv_std::arch::f_negate_vector(vector) == glam::Vec2::new(5.0, 0.0));
}
