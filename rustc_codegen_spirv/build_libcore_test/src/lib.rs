#![no_std]
#![feature(register_attr)]
#![register_attr(spirv)]

use core::panic::PanicInfo;
use spirv_std::Private;

#[no_mangle]
pub extern "C" fn screaming_bananans(mut x: Private<u32>) {
    x.store(x.load() + 1);
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
