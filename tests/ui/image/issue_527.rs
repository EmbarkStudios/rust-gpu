use glam::*;

#[spirv(block)]
pub struct PointsBuffer {
    points: [UVec2; 100],
}

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(global_invocation_id)] id: UVec3,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] points_buffer: &mut PointsBuffer,
    #[spirv(uniform_constant, descriptor_set = 1, binding = 1)] image: &spirv_std::StorageImage2d,
) {
    unsafe { asm!("OpCapability StorageImageWriteWithoutFormat") };
    let position = id.xy();
    for i in 0..100usize {
        let p0 = &points_buffer.points[i];
        let p1 = &points_buffer.points[i + 1];
        if p0.x == position.x && p1.y == position.y {
            unsafe {
                image.write(position, vec2(1.0, 0.0));
            };
        }
    }
}
