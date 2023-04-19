// FIXME(eddyb) this is like `member_ref_arg`, but testing the error messages
// in some broken cases - this test should eventually pass, but for now
// we care more that the error messages do not regress too much.

// build-fail
// normalize-stderr-not_spirt "36\[%36\]" -> "$$ID[%$$ID]"
// normalize-stderr-spirt     "38\[%38\]" -> "$$ID[%$$ID]"
// normalize-stderr-test "(note: module `.*)\.(not_spirt|spirt)`" -> "$1.{not_spirt,spirt}`"

use spirv_std::spirv;

struct S {
    x: u32,
    y: u32,
}

// NOTE(eddyb) `#[inline(never)]` is for blocking inlining at the e.g. MIR level,
// whereas any Rust-GPU-specific legalization will intentially ignore it.

#[inline(never)]
fn f(x: &u32) -> u32 {
    *x
}

#[inline(never)]
fn g(xy: (&u32, &u32)) -> (u32, u32) {
    (*xy.0, *xy.1)
}

#[inline(never)]
fn h(xyz: (&u32, &u32, &u32)) -> (u32, u32, u32) {
    (*xyz.0, *xyz.1, *xyz.2)
}

// FIXME(eddyb) the reason this doesn't work while the others do, is somewhat
// subtle - specifically, we currently have some passes (`destructure_composites`
// and `spirt_passes::reduce`) which can handle single-level `OpComposite*`
// instructions, but the extra nesting here stops them dead in their tracks
// (they're also not really the right solution for this problem, such composites
// should just never exist by-value, and `qptr` may very well get rid of them).
#[inline(never)]
fn h_newtyped(xyz: ((&u32, &u32, &u32),)) -> (u32, u32, u32) {
    (*xyz.0 .0, *xyz.0 .1, *xyz.0 .2)
}

#[spirv(fragment)]
pub fn main() {
    let s = S { x: 2, y: 2 };
    f(&s.x);
    g((&s.x, &s.y));
    h((&s.x, &s.y, &s.x));
    h_newtyped(((&s.x, &s.y, &s.x),));
}
