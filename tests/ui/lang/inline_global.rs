// build-pass
use spirv_std as _;

#[inline(never)]
fn sdf(con: &mut [u32]) -> f32{
    (con[1] - con[0]) as f32
}

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] runtime_array: &mut [u32],
) {
    sdf(runtime_array);
}
