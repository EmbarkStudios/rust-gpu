// Tests validating tracking of basic blocks
// within the `asm!` macro.
// build-fail

use core::arch::asm;
use spirv_std::spirv;

// Active basic block with `noreturn`.
fn asm_noreturn_open() {
    unsafe {
        asm!("", options(noreturn));
    }
}

// No active basic block without `noreturn`.
fn asm_closed() {
    unsafe {
        asm!("OpUnreachable");
    }
}

// Invalid op after terminator
fn asm_invalid_op_terminator(x: f32) {
    unsafe {
        asm!(
            "OpKill",
            "%sum = OpFAdd _ {x} {x}",
            x = in(reg) x,
        );
    }
}

#[spirv(fragment)]
pub fn main() {
    asm_closed();
    asm_noreturn_open();
    asm_invalid_op_terminator(1.0);
}
