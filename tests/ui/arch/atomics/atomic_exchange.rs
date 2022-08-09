// build-pass

#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
use spirv_std::{
    arch::IndexUnchecked,
    memory::{Scope, Semantics},
};

#[spirv(compute(threads(64)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &mut [u32]) {
    unsafe {
        let old = spirv_std::arch::atomic_exchange::<
            _,
            { Scope::CrossDevice as u32 },
            { Semantics::UNIFORM_MEMORY.bits() as u32 },
        >(&mut *buffer.index_unchecked_mut(0), 5);
    }
}
