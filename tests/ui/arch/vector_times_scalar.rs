// build-pass

#[spirv(fragment)]
pub fn main() {
    let vector = glam::Vec2::new(10.0, 10.0);
    let scalar = 2.0;
    assert!(spirv_std::arch::vector_times_scalar(vector, scalar) == glam::Vec2::new(20.0, 20.0));
}
