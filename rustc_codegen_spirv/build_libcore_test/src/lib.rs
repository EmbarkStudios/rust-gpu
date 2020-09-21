#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::panic::PanicInfo;
use spirv_std::{CrossWorkgroup, UniformConstant};

#[allow(unused_attributes)]
#[spirv(entry = "kernel")]
pub fn add_two_ints(x: UniformConstant<u32>, y: UniformConstant<u32>, mut z: CrossWorkgroup<u32>) {
    z.store(x.load() + y.load())
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
