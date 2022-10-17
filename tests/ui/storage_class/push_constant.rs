// Test that using push constants passes (Vulkan) validation.

// build-pass
use spirv_std::spirv;

#[derive(Copy, Clone)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

#[spirv(fragment)]
pub fn main(#[spirv(push_constant)] constants: &ShaderConstants) {
    let _constants = *constants;
}
