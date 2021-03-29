// Test `OpImageWrite`
// build-pass

use spirv_std::{arch, StorageImage2d};

#[spirv(fragment)]
pub fn main(
    texels: glam::Vec2,
    #[spirv(uniform_constant, descriptor_set = 0, binding = 0)] image: &StorageImage2d,
) {
    unsafe {
        asm!("OpCapability StorageImageWriteWithoutFormat");
        image.write(glam::UVec2::new(0, 1), texels);
    }
}
