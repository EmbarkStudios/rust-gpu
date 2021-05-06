// build-pass

#[spirv(fragment)]
pub fn main() {
    let x = glam::Vec2::new(1.5, 2.5);
    let y = glam::Vec2::new(2.5, 3.5);
    let z = glam::Vec2::new(1.5, 2.5);

    let result: glam::BVec2 = spirv_std::arch::f_ord_equal_vector(x, z);
    let result2: glam::BVec2 = spirv_std::arch::f_ord_equal_vector(x, z);
}
