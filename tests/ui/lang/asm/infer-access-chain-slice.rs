// Tests that `asm!` can infer the result type of `OpAccessChain`,
// when used to index slices.

// build-pass

use core::arch::asm;
use glam::Vec4;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] slice_in: &[Vec4],
    #[spirv(flat)] i: u32,
    out: &mut Vec4,
) {
    unsafe {
        asm!(
            // HACK(eddyb) we can't pass in the `&[T]` to `asm!` directly,
            // and `as *const T` casts would require some special-casing
            // to avoid actually going through an `OpTypePointer T`, so
            // instead we extract the data pointer in the `asm!` itself.
            "%slice_ptr = OpLoad _ {slice_ptr_ptr}",
            "%data_ptr = OpCompositeExtract _ %slice_ptr 0",
            "%val_ptr = OpAccessChain _ %data_ptr {index}",
            "%val = OpLoad _ %val_ptr",
            "OpStore {out_ptr} %val",
            slice_ptr_ptr = in(reg) &slice_in,
            index = in(reg) i,
            out_ptr = in(reg) out,
        );
    }
}
