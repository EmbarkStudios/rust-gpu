// Test that various packing methods work.
// build-pass

use spirv_std::float::*;
use spirv_std::glam::{Vec2, Vec4};

#[rust_gpu::spirv(fragment)]
pub fn test_vec2_to_f16x2(i: Vec2, #[rust_gpu::spirv(flat)] o: &mut u32) {
    *o = vec2_to_f16x2(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_f16x2_to_vec2(#[rust_gpu::spirv(flat)] i: u32, o: &mut Vec2) {
    *o = f16x2_to_vec2(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_f32_to_f16(i: f32, #[rust_gpu::spirv(flat)] o: &mut u32) {
    *o = f32_to_f16(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_f16_to_f32(#[rust_gpu::spirv(flat)] i: u32, o: &mut f32) {
    *o = f16_to_f32(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_vec4_to_u8x4_snorm(i: Vec4, #[rust_gpu::spirv(flat)] o: &mut u32) {
    *o = vec4_to_u8x4_snorm(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_vec4_to_u8x4_unorm(i: Vec4, #[rust_gpu::spirv(flat)] o: &mut u32) {
    *o = vec4_to_u8x4_unorm(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_vec2_to_u16x2_snorm(i: Vec2, #[rust_gpu::spirv(flat)] o: &mut u32) {
    *o = vec2_to_u16x2_snorm(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_vec2_to_u16x2_unorm(i: Vec2, #[rust_gpu::spirv(flat)] o: &mut u32) {
    *o = vec2_to_u16x2_unorm(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_u8x4_to_vec4_snorm(#[rust_gpu::spirv(flat)] i: u32, o: &mut Vec4) {
    *o = u8x4_to_vec4_snorm(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_u8x4_to_vec4_unorm(#[rust_gpu::spirv(flat)] i: u32, o: &mut Vec4) {
    *o = u8x4_to_vec4_unorm(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_u16x2_to_vec2_snorm(#[rust_gpu::spirv(flat)] i: u32, o: &mut Vec2) {
    *o = u16x2_to_vec2_snorm(i);
}

#[rust_gpu::spirv(fragment)]
pub fn test_u16x2_to_vec2_unorm(#[rust_gpu::spirv(flat)] i: u32, o: &mut Vec2) {
    *o = u16x2_to_vec2_unorm(i);
}
