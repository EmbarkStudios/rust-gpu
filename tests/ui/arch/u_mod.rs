// build-pass

#[spirv(fragment)]
pub fn main() {
    let x: u32 = 10;
    let y = 2;
    let vx = glam::UVec2::new(10, 10);
    let vy = glam::UVec2::new(2, 2);
    assert!(spirv_std::arch::u_mod_vector(vx, vy) == glam::UVec2::new(0, 0));
}
