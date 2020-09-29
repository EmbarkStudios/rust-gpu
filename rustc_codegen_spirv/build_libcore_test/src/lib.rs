#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::panic::PanicInfo;
#[cfg(not(target_feature = "shader"))]
use spirv_std::CrossWorkgroup;
#[cfg(target_feature = "shader")]
use spirv_std::{f32x4, Input, Output};

#[cfg(target_feature = "shader")]
#[allow(unused_attributes)]
#[spirv(entry = "fragment")]
pub fn main(input: Input<f32x4>, mut output: Output<f32x4>) {
    output.store(input.load())
}

#[cfg(not(target_feature = "shader"))]
#[allow(unused_attributes)]
#[spirv(entry = "kernel")]
pub fn add_two_ints(x: CrossWorkgroup<u32>, y: CrossWorkgroup<u32>, mut z: CrossWorkgroup<u32>) {
    z.store(x.load() + y.load())
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
