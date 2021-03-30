// Test that using DST (i.e. slice) storage buffers passes (Vulkan) validation.

// build-pass
use spirv_std as _;

// `Block` decoration is required for storage buffers when compiling for Vulkan.
#[cfg_attr(not(target_env = "unknown"), spirv(block))]
pub struct SliceF32 {
    rta: [f32],
}

#[spirv(fragment)]
pub fn main(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] slice: &mut SliceF32) {
    let float: f32 = slice.rta[0];
    let _ = float;
}
