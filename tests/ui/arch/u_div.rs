// build-pass

#[spirv(fragment)]
pub fn main() {
    let x: u32 = 10;
    let y = 2;
    let vx = glam::UVec2::new(10, 10);
    let vy = glam::UVec2::new(2, 2);
    assert!(unsafe { spirv_std::arch::u_div_vector(vx, vy) } == glam::UVec2::new(5, 5));
}
