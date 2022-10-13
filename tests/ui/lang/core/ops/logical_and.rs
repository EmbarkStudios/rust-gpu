// Test using `&&` operator.
// build-pass

use spirv_std as _;

fn f(x: bool, y: bool) -> bool {
    x && y
}

#[rust_gpu::spirv(fragment)]
pub fn main() {
    f(false, true);
}
