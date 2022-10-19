// Tests that `asm!` can infer the result type of `OpAccessChain`,
// when used to index arrays.

// build-pass

use core::arch::asm;
use glam::Vec4;
use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(push_constant)] array_in: &[Vec4; 16],
    #[rust_gpu::spirv(flat)] i: u32,
    out: &mut Vec4,
) {
    unsafe {
        asm!(
            "%val_ptr = OpAccessChain _ {array_ptr} {index}",
            "%val = OpLoad _ %val_ptr",
            "OpStore {out_ptr} %val",
            array_ptr = in(reg) array_in,
            index = in(reg) i,
            out_ptr = in(reg) out,
        );
    }
}
