// build-pass

#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
use spirv_std::{
    arch::IndexUnchecked,
    memory::{Scope, Semantics},
};

#[spirv(compute(threads(64)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &mut [u32]) {
    let old = unsafe {
        spirv_std::arch::atomic_u_min::<
            _,
            { Scope::Workgroup as u8 },
            { Semantics::UNIFORM_MEMORY.bits() as u8 },
        >(&mut *buffer.index_unchecked_mut(0), 5)
    };
}
