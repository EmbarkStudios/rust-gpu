// Test that using push constants work.
// NOTE(eddyb) this won't pass Vulkan validation (see `push_constant_vulkan`),
// but should still pass the baseline SPIR-V validation.

// build-pass

extern crate spirv_std;
use spirv_std::storage_class::PushConstant;


#[derive(Copy, Clone)]
pub struct ShaderConstants {
    pub width: u32,
    pub height: u32,
    pub time: f32,
}

#[spirv(fragment)]
pub fn main(constants: PushConstant<ShaderConstants>) {
    let _constants = *constants;
}
