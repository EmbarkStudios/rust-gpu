// build-pass

#![feature(const_generics)]
#![allow(incomplete_features)]

use spirv_std::{memory::{Scope, Semantics}, storage_class::Image};

#[spirv(fragment)]
pub fn main(output: Image<f32>) {
    unsafe {
        let output = spirv_std::arch::atomic_load::<
            _,
            { Scope::CrossDevice },
            { Semantics::ImageMemory }
        >(&*output);
    }
}
