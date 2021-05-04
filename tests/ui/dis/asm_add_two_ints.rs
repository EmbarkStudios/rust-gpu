// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=asm_add_two_ints::add_two_ints

use spirv_std as _;

fn add_two_ints(x: u32, y: u32) -> u32 {
    let result;
    unsafe {
        asm!(
            "{0} = OpIAdd typeof{0} {1} {2}",
            out(reg) result,
            in(reg) x,
            in(reg) y,
        );
    }
    result
}
#[spirv(fragment)]
pub fn main() {
    add_two_ints(2, 3);
}
