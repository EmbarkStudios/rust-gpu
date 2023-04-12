// build-pass
// compile-flags: -Ctarget-feature=+StorageImageExtendedFormats

use glam::{Vec2, Vec3, Vec4};
use spirv_std::spirv;
use spirv_std::{arch, Image};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image1: &Image!(2D, type=f32, sampled, components=1),
    #[spirv(descriptor_set = 0, binding = 1)] image2: &Image!(2D, type=f32, sampled, components=2),
    #[spirv(descriptor_set = 0, binding = 2)] image3: &Image!(2D, type=f32, sampled, components=3),
    #[spirv(descriptor_set = 0, binding = 3)] image4: &Image!(2D, type=f32, sampled),
    #[spirv(descriptor_set = 0, binding = 4)] image2_implied: &Image!(2D, format = rg16f, sampled),
    #[spirv(descriptor_set = 0, binding = 5)] image3_implied: &Image!(
        2D,
        format = r11f_g11f_b10f,
        sampled
    ),
    output: &mut glam::Vec4,
) {
    let coords = glam::IVec2::new(0, 1);
    let t1: f32 = image1.fetch(coords);
    let t2: Vec2 = image2.fetch(coords);
    let t3: Vec3 = image3.fetch(coords);
    let t4: Vec4 = image4.fetch(coords);
    let t5: Vec2 = image2_implied.fetch(coords);
    let t6: Vec3 = image3_implied.fetch(coords);
    *output = Vec4::splat(t1) + ((t2 + t5).extend(0.0) + t3 + t6).extend(0.0) + t4;
}
