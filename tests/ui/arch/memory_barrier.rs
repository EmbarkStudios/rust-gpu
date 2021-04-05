// build-pass

#![feature(const_generics)]
#![allow(incomplete_features)]

use spirv_std::memory::{Scope, Semantics};

#[spirv(fragment)]
pub fn main() {
    unsafe {
        spirv_std::arch::memory_barrier::<
            { Scope::Subgroup },
            { (Semantics::AcquireRelease as u32) | (Semantics::UniformMemory as u32) },
        >();
    }
}
