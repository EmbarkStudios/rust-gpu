// build-pass

use spirv_std::spirv;

struct S {
    x: u32,
    y: u32,
}

fn f(x: &u32) {}

#[spirv(fragment)]
pub fn main() {
    let s = S { x: 2, y: 2 };
    f(&s.x);
}
