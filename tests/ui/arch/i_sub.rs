// build-pass

#[spirv(fragment)]
pub fn main() {
    let x = 5;
    let y = 5;
    let vx = glam::IVec2::new(5, 7);
    let vy = glam::IVec2::new(5, 7);
    assert!(spirv_std::arch::i_sub_vector(vx, vy) == glam::IVec2::new(0, 0));
}
