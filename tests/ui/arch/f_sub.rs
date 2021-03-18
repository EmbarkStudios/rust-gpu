// build-pass

#[spirv(fragment)]
pub fn main() {
    let x = 5.0;
    let y = 5.0;
    let vx = glam::Vec2::new(5.0, 7.0);
    let vy = glam::Vec2::new(5.0, 7.0);
    assert!(spirv_std::arch::f_sub_vector(vx, vy) == glam::Vec2::new(0.0, 0.0));
}
