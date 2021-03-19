// build-pass

#[spirv(fragment)]
pub fn main() {
    let x = -10.0;
    let y = -2.0;
    let vx = glam::Vec2::new(-10.0, -10.0);
    let vy = glam::Vec2::new(-2.0, -2.0);
    assert!(spirv_std::arch::f_mod_vector(vx, vy) == glam::Vec2::new(-0.0, -0.0));
}
