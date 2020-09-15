#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::panic::PanicInfo;
use spirv_std::Workgroup;

#[allow(unused_attributes)]
#[spirv(entry = "kernel")]
pub fn screaming_bananans(mut x: Workgroup<u32>) {
    x.store(x.load() + 1);
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
