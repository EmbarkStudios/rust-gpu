use spirv_std::glam::Vec4;
use spirv_std::spirv;
use spirv_std::{image::Image2dArray, Sampler};

#[spirv(fragment)]
pub fn ps_main_stereo(
    output: &mut Vec4,
    #[spirv(descriptor_set = 0, binding = 0)] in_texture: &Image2dArray,
) {
    let mut color = Vec4::splat(0.0);

    let mut n = 0;
    while n < 10 {
        let x = *in_texture;
    }

    *output = color;
}
