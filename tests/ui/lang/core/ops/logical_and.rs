// Test using `&&` operator.
// build-pass

extern crate spirv_std;

fn f(x: bool, y: bool) -> bool {
    x && y
}

#[spirv(fragment)]
pub fn main() {
    f(false, true);
}
