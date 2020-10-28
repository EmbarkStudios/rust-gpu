#![cfg_attr(target_arch = "spirv", no_std)]
#![feature(lang_items)]
#![feature(register_attr)]
#![register_attr(spirv)]

#[cfg(not(test))]
use core::panic::PanicInfo;
extern crate spirv_std;

use spirv_std::Input;

#[spirv(gl_compute, local_size_x=64)]
pub fn main_cs(y: Input<f32>) {
    
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

