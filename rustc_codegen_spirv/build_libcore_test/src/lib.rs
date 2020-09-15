#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::panic::PanicInfo;

#[spirv(storage_class = "private")]
pub struct Private<'a, T> {
    x: &'a T,
}

#[no_mangle]
pub extern "C" fn screaming_bananans(x: Private<u32>) {}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
