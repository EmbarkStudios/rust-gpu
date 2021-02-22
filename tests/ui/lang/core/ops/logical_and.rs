// Test using `&&` operator.
// build-pass

use spirv_std as _;

fn f(x: bool, y: bool) -> bool {
    x && y
}

#[spirv(fragment)]
pub fn main() {
    f(false, true);
}
