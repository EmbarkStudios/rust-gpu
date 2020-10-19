#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::panic::PanicInfo;
use spirv_std::{f32x4, Input, Output};

#[allow(unused_attributes)]
#[spirv(entry = "fragment")]
pub fn main(input: Input<f32x4>, mut output: Output<f32x4>) {
    output.store(input.load());
}

#[allow(unused_attributes)]
#[spirv(entry = "vertex")]
pub fn main_vs(
    in_pos: Input<f32x4>,
    in_color: Input<f32x4>,
    #[spirv(builtin = "position")] mut out_pos: Output<f32x4>,
    mut out_color: Output<f32x4>,
) {
    out_pos.store(in_pos.load());
    out_color.store(in_color.load());
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
