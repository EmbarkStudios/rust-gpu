// build-pass

use spirv_std::{
    arch::IndexUnchecked,
    memory::{Scope, Semantics},
};

#[cfg(target_arch = "spirv")]
use core::arch::asm;

#[spirv(compute(threads(64)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &mut [u32]) {
    // Ensure kernel capabilities.
    unsafe { asm!("OpCapability Kernel") };

    let old = unsafe {
        spirv_std::arch::atomic_flag_test_and_set::<
            _,
            { Scope::Workgroup as u32 },
            { Semantics::UNIFORM_MEMORY.bits() },
        >(&mut *buffer.index_unchecked_mut(0))
    };
}
