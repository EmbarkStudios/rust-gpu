// build-pass
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::{arch, Image};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled),
    #[spirv(flat)] output: &mut u32,
) {
    *output = image.query_levels();
}
