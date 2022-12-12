// HACK(eddyb) duplicate of issue-373.spirt.rs because only-/ignore- do not work with revisions.
// only-not_spirt

// Test that returning a single-scalar-field `#[repr(C)] struct` doesn't generate
// unsupported pointer casts (the problem was the use of `PassMode::Cast`, through
// the default Rust ABI adjustments, that we now override through query hooks).

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::spirv;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct S {
    x: f32,
}

fn f() -> S {
    S { x: 2.0 }
}

#[spirv(fragment)]
pub fn main(out: &mut f32) {
    *out = f().x;
}
