//! Ported to Rust from https://github.com/Tw1ddle/Sky-Shader/blob/master/src/shaders/glsl/sky.fragment

#![no_std]
#![feature(lang_items)]
#![feature(register_attr)]
#![register_attr(spirv)]

#[cfg(not(test))]
use core::panic::PanicInfo;
use spirv_std::{Input, Output, Vec4};

#[allow(unused_attributes)]
#[spirv(entry = "fragment")]
pub fn main_fs(mut output: Output<Vec4>) {
    output.store(Vec4::new(1.0, 0.0, 0.0, 1.0))
}

#[allow(unused_attributes)]
#[spirv(entry = "vertex")]
pub fn main_vs(
    #[spirv(builtin = "vertex_index")] vert_id: Input<i32>,
    #[spirv(builtin = "position")] mut out_pos: Output<Vec4>,
) {
    let vert_id = vert_id.load();
    out_pos.store(Vec4::new(
        (vert_id - 1) as f32,
        ((vert_id & 1) * 2 - 1) as f32,
        0.0,
        1.0,
    ));
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

#[cfg(not(test))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}
