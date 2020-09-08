#![no_std]

use core::panic::PanicInfo;

pub fn screaming_bananans() {}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
