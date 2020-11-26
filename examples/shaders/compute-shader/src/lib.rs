#![cfg_attr(target_arch = "spirv", no_std)]
#![feature(register_attr)]
#![register_attr(spirv)]

extern crate spirv_std;

#[allow(unused_attributes)]
#[spirv(gl_compute)]
pub fn main_cs() {}
