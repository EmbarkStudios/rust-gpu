// Tests that we don't allow returning from `asm!` (which would always be UB).
// build-fail

use core::arch::asm;
use spirv_std::spirv;

fn asm_return() {
    unsafe {
        asm!("OpReturn", options(noreturn));
    }
}

fn asm_return_value(x: u32) -> u32 {
    unsafe {
        asm!(
            "OpReturnValue {x}",
            x = in(reg) x,
            options(noreturn),
        );
    }
}

fn asm_return_label() {
    unsafe {
        asm!(
            "OpReturn",          // close active block
            "%unused = OpLabel", // open new block
        );
    }
}

fn asm_return_value_label(x: u32) -> u32 {
    unsafe {
        asm!(
            "OpReturnValue {x}", // close active block
            "%unused = OpLabel", // open new block
            x = in(reg) x
        );
    }
    0
}

#[spirv(fragment)]
pub fn main() {
    asm_return();
    asm_return_value(123);
    asm_return_label();
    asm_return_value_label(123);
}
