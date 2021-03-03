// build-pass

#[spirv(fragment)]
pub fn main() {
    let x = 5;
    let y = 2;
    let vx = glam::IVec2::new(2, 5);
    let vy = glam::IVec2::new(5, 2);
    assert!(unsafe { spirv_std::arch::i_add_vector(vx, vy) } == glam::IVec2::new(7, 7));
}
