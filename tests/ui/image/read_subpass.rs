// build-pass
// compile-flags: -C target-feature=+InputAttachment

use spirv_std::spirv;
use spirv_std::{arch, Image};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, input_attachment_index = 0)] image: &Image!(subpass, type=f32, sampled=false),
    output: &mut glam::Vec4,
) {
    let coords = image.read_subpass(glam::IVec2::new(0, 0));
    *output = coords;
}
