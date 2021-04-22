// build-pass

#![feature(const_generics)]
#![allow(incomplete_features)]

use spirv_std::memory::{Scope, Semantics};

#[spirv(fragment)]
pub fn main() {
    unsafe {
        spirv_std::arch::control_barrier::<
            { Scope::Subgroup },
            { Scope::Subgroup },
            { Semantics::NONE },
        >();
    }
}
