// build-pass

use spirv_std as _;
struct A;
struct B;

struct S {
    x: A,
    y: B,
}

fn f(x: &B) {}

#[spirv(fragment)]
pub fn main() {
    let s = S { x: A, y: B };
    f(&s.y);
}
