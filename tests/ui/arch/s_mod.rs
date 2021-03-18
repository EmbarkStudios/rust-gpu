// build-pass

#[spirv(fragment)]
pub fn main() {
    let x = 10;
    let y = 2;
    let vx = glam::IVec2::new(10, 10);
    let vy = glam::IVec2::new(2, 2);
    assert!(spirv_std::arch::s_mod_vector(vx, vy) == glam::IVec2::new(0, 0));
}
