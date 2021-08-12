// Tests validating tracking of basic blocks
// within the `asm!` macro.
// build-pass

use spirv_std as _;

fn asm_label() {
    unsafe {
        asm!(
            "%unused = OpLabel",
        );
    }
}

fn asm_noreturn_single() -> ! {
    unsafe {
        asm!(
            "OpKill",
            options(noreturn),
        );
    }
}

#[spirv(fragment)]
pub fn main() {
    asm_label();
    // asm_noreturn_single();
}
