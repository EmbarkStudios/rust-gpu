// build-pass

#![feature(adt_const_params)]
#![allow(incomplete_features)]

use spirv_std::memory::{Scope, Semantics};

#[spirv(fragment)]
pub fn main() {
    unsafe {
        spirv_std::arch::memory_barrier::<
            { Scope::Subgroup as u32 },
            { Semantics::ACQUIRE_RELEASE.bits() | Semantics::UNIFORM_MEMORY.bits() },
        >();
    }
}
