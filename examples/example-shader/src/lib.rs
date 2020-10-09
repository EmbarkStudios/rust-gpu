#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::panic::PanicInfo;
use spirv_std::{f32x4, Input, Output};

#[allow(unused_attributes)]
#[spirv(entry = "fragment")]
pub fn main(input: Input<f32x4>, mut output: Output<f32x4>) {
    output.store(input.load())
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
