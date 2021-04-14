// Test `OpImageRead`
// build-pass

use spirv_std::{arch, Image};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled=false),
    output: &mut glam::Vec4,
) {
    unsafe { asm!("OpCapability StorageImageReadWithoutFormat") };
    let coords = image.read(glam::IVec2::new(0, 1));
    *output = coords;
}
