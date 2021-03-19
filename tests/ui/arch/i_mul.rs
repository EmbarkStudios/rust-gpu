// build-pass

#[spirv(fragment)]
pub fn main() {
    let x = 5;
    let y = 2;
    let vx = glam::IVec2::new(5, 2);
    let vy = glam::IVec2::new(2, 5);
    assert!(spirv_std::arch::i_mul_vector(vx, vy) == glam::IVec2::new(10, 10));
}
