// Test `OpImageRead`
// build-pass

use spirv_std::{arch, StorageImage2d};

#[spirv(fragment)]
pub fn main(
    #[spirv(uniform_constant, descriptor_set = 0, binding = 0)] image: &StorageImage2d,
    output: &mut glam::Vec4,
) {
    unsafe { asm!("OpCapability StorageImageReadWithoutFormat") };
    let coords = image.read(glam::IVec2::new(0, 1));
    *output = coords;
}
