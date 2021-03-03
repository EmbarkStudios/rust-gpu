// Tests using `asm!` with a const argument.
// build-pass

use spirv_std as _;

fn asm() {
    unsafe {
        const N: usize = 3;
        asm!(
            "%int = OpTypeInt 32 0",
            "%type = OpTypeVector %int {len}",
            len = const N,
        );
    }
}

#[spirv(fragment)]
pub fn main() {
    asm();
}
