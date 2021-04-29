// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=asm::asm

use spirv_std as _;

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
