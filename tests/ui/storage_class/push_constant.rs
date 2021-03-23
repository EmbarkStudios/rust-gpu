// Test that using push constants work.
// NOTE(eddyb) this won't pass Vulkan validation (see `push_constant_vulkan`),
// but should still pass the baseline SPIR-V validation.

// build-pass

use spirv_std as _;

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
