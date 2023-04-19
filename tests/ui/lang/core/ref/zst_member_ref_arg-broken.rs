// FIXME(eddyb) this is like `zst_member_ref_arg`, but testing the error messages
// in some broken cases (see issue #1037) - this test should eventually pass, but
// for now we care more that the error messages do not regress too much.

// build-fail

use spirv_std::spirv;
struct A;
struct B;

pub struct S<Z, W = ()> {
    x: A,
    y: B,

    z: Z,
    w: W,
}

fn f(x: &B) {}

#[spirv(fragment)]
pub fn main_scalar(#[spirv(push_constant)] s: &S<usize>) {
    f(&s.y);
}

#[spirv(fragment)]
pub fn main_scalar_pair(#[spirv(push_constant)] s: &S<usize, usize>) {
    f(&s.y);
}

#[spirv(fragment)]
pub fn main_scalar_scalar_pair_nested(#[spirv(push_constant)] s: &S<(usize, usize)>) {
    f(&s.y);
}
