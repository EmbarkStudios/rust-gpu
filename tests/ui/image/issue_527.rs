// build-pass
// compile-flags: -C target-feature=+StorageImageWriteWithoutFormat

use glam::*;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(global_invocation_id)] id: UVec3,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] points_buffer: &mut [UVec2; 100],
    #[spirv(descriptor_set = 1, binding = 1)] image: &spirv_std::Image!(2D, type=f32, sampled=false),
) {
    let position = id.xy();
    for i in 0..100usize {
        let p0 = &points_buffer[i];
        let p1 = &points_buffer[i + 1];
        if p0.x == position.x && p1.y == position.y {
            unsafe {
                image.write(position, vec2(1.0, 0.0));
            };
        }
    }
}
