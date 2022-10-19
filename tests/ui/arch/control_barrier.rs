// build-pass

#![feature(adt_const_params)]
#![allow(incomplete_features)]

use spirv_std::memory::{Scope, Semantics};

#[rust_gpu::spirv(fragment)]
pub fn main() {
    unsafe {
        spirv_std::arch::control_barrier::<
            { Scope::Subgroup as u32 },
            { Scope::Subgroup as u32 },
            { Semantics::NONE.bits() },
        >();
    }
}
