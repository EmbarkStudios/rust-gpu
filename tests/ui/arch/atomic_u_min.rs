// build-pass

#![feature(const_generics)]
#![allow(incomplete_features)]

use spirv_std::{memory::{Scope, Semantics}, storage_class::Image};

#[spirv(fragment)]
pub fn main(mut output: Image<u32>) {
    unsafe {
        let old = spirv_std::arch::atomic_u_max::<
            _,
            { Scope::CrossDevice },
            { Semantics::ImageMemory },
        >(&mut *output, 10);
    }
}
