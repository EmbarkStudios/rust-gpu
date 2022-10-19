// Test `OpVectorExtractDynamic`
// build-pass

use spirv_std::arch;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    let vector = glam::Vec2::new(1.0, 2.0);
    let element = unsafe { arch::vector_extract_dynamic(vector, 1) };
    assert!(2.0 == element);
    let uvector = glam::UVec2::new(1, 2);
    let uelement = unsafe { arch::vector_extract_dynamic(uvector, 1) };
    assert!(2 == uelement);
}
