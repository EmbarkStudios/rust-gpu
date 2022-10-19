// build-pass

use spirv_std::arch::IndexUnchecked;
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &mut [u32]) {
    let reference = unsafe { buffer.index_unchecked_mut(0) };

    let old = unsafe {
        spirv_std::arch::atomic_i_increment::<
            _,
            { spirv_std::memory::Scope::Workgroup as u32 },
            { spirv_std::memory::Semantics::NONE.bits() as u32 },
        >(reference)
    };
    assert!(old == 0);
}
