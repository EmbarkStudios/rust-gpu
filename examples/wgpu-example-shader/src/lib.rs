#![cfg_attr(target_arch = "spirv", no_std)]
#![feature(lang_items)]
#![feature(register_attr)]
#![register_attr(spirv)]

use spirv_std::{Input, Output, Vec4};

#[cfg(all(not(target_arch = "spirv"), feature = "build-spirv"))]
pub const COMPILED_SHADER: &'static [u8] = include_bytes!(env!("wgpu_example_shader.spv"));

#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main_fs(mut output: Output<Vec4>) {
    output.store(Vec4::new(1.0, 0.0, 0.0, 1.0))
}

#[allow(unused_attributes)]
#[spirv(vertex)]
pub fn main_vs(
    #[spirv(vertex_index)] vert_id: Input<i32>,
    #[spirv(position)] mut out_pos: Output<Vec4>,
) {
    let vert_id = vert_id.load();
    out_pos.store(Vec4::new(
        (vert_id - 1) as f32,
        ((vert_id & 1) * 2 - 1) as f32,
        0.0,
        1.0,
    ));
}

#[cfg(all(not(test), target_arch = "spirv"))]
#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[cfg(all(not(test), target_arch = "spirv"))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}
