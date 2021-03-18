// Doesn't work, only worked before because I think it got optimized away before
// hitting the backend.

// build-fail

#![feature(ptr_internals)]

use spirv_std::storage_class::Output;

use core::ptr::Unique;
const POINTER: Unique<[u8;4]> = Unique::<[u8; 4]>::dangling();

#[spirv(fragment)]
pub fn main(mut output: Output<u8>) {
    let _pointer = POINTER;
}
