// Test `OpVectorInsertDynamic`
// build-pass

use spirv_std::arch;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    let vector = glam::Vec2::new(1.0, 2.0);
    let expected = glam::Vec2::new(1.0, 3.0);
    let new_vector = unsafe { arch::vector_insert_dynamic(vector, 1, 3.0) };
    assert!(new_vector == expected);
    let uvector = glam::UVec2::new(1, 2);
    let uexpected = glam::UVec2::new(1, 3);
    let unew_vector = unsafe { arch::vector_insert_dynamic(uvector, 1, 3) };
    assert!(unew_vector == uexpected);
}
