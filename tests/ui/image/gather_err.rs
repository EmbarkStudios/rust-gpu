// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// compile-flags: -Ctarget-feature=+Sampled1D

use spirv_std::{arch, spirv, Image, Sampler};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image1d: &Image!(1D, type=f32, sampled),
    #[spirv(descriptor_set = 2, binding = 1)] image3d: &Image!(3D, type=f32, sampled),
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    let r1: glam::Vec4 = image1d.gather(*sampler, 0.0f32, 0);
    let r2: glam::Vec4 = image3d.gather(*sampler, v3, 0);
    *output = r1 + r2;
}
