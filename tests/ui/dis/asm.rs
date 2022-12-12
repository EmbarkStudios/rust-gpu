// normalize-stderr-not_spirt "OpLine %5 18 1" -> "OpNoLine"

// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=asm::asm

use core::arch::asm;
use spirv_std::spirv;

fn asm() {
    unsafe {
        asm!(
            "%int = OpTypeInt 32 0",
            "%scope = OpConstant %int 3",
            "%semantics = OpConstant %int 72",
            "OpMemoryBarrier %scope %semantics",
        );
    }
}
#[spirv(fragment)]
pub fn main() {
    asm();
}
