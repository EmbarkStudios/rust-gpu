// Test that various packing methods work.
// build-pass

use spirv_std::float::*;
use spirv_std::glam::{Vec2, Vec4};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn test_vec2_to_f16x2(i: Vec2, o: &mut u32) {
    *o = vec2_to_f16x2(i);
}

#[spirv(fragment)]
pub fn test_f16x2_to_vec2(#[spirv(flat)] i: u32, o: &mut Vec2) {
    *o = f16x2_to_vec2(i);
}

#[spirv(fragment)]
pub fn test_f32_to_f16(i: f32, o: &mut u32) {
    *o = f32_to_f16(i);
}

#[spirv(fragment)]
pub fn test_f16_to_f32(#[spirv(flat)] i: u32, o: &mut f32) {
    *o = f16_to_f32(i);
}

#[spirv(fragment)]
pub fn test_vec4_to_u8x4_snorm(i: Vec4, o: &mut u32) {
    *o = vec4_to_u8x4_snorm(i);
}

#[spirv(fragment)]
pub fn test_vec4_to_u8x4_unorm(i: Vec4, o: &mut u32) {
    *o = vec4_to_u8x4_unorm(i);
}

#[spirv(fragment)]
pub fn test_vec2_to_u16x2_snorm(i: Vec2, o: &mut u32) {
    *o = vec2_to_u16x2_snorm(i);
}

#[spirv(fragment)]
pub fn test_vec2_to_u16x2_unorm(i: Vec2, o: &mut u32) {
    *o = vec2_to_u16x2_unorm(i);
}

#[spirv(fragment)]
pub fn test_u8x4_to_vec4_snorm(#[spirv(flat)] i: u32, o: &mut Vec4) {
    *o = u8x4_to_vec4_snorm(i);
}

#[spirv(fragment)]
pub fn test_u8x4_to_vec4_unorm(#[spirv(flat)] i: u32, o: &mut Vec4) {
    *o = u8x4_to_vec4_unorm(i);
}

#[spirv(fragment)]
pub fn test_u16x2_to_vec2_snorm(#[spirv(flat)] i: u32, o: &mut Vec2) {
    *o = u16x2_to_vec2_snorm(i);
}

#[spirv(fragment)]
pub fn test_u16x2_to_vec2_unorm(#[spirv(flat)] i: u32, o: &mut Vec2) {
    *o = u16x2_to_vec2_unorm(i);
}
