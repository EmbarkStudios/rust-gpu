// build-pass

use spirv_std::spirv;

struct S {
    x: u32,
    y: u32,
}

// NOTE(eddyb) `#[inline(never)]` is for blocking inlining at the e.g. MIR level,
// whereas any Rust-GPU-specific legalization will intentially ignore it.

#[inline(never)]
fn f(x: &u32) {}

#[inline(never)]
fn g(xy: (&u32, &u32)) {}

#[spirv(fragment)]
pub fn main() {
    let s = S { x: 2, y: 2 };
    f(&s.x);
    g((&s.x, &s.y));
}
