// Test that using push constants passes (Vulkan) validation.

// build-pass
use spirv_std as _;

#[derive(Copy, Clone)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

#[rust_gpu::spirv(fragment)]
pub fn main(#[rust_gpu::spirv(push_constant)] constants: &ShaderConstants) {
    let _constants = *constants;
}
