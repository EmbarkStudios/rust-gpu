// build-pass

use spirv_std::{
    arch::IndexUnchecked,
    memory::{Scope, Semantics},
};

#[spirv(compute(threads(64)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &mut [u32]) {
    let old = unsafe {
        spirv_std::arch::atomic_xor::<
            _,
            { Scope::Workgroup as u32 },
            { Semantics::UNIFORM_MEMORY.bits() },
        >(&mut *buffer.index_unchecked_mut(0), 5)
    };
}
