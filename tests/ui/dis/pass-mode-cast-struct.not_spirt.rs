// HACK(eddyb) duplicate of pass-mode-cast-struct.spirt.rs because only-/ignore- do not work with revisions.
// only-not_spirt

// Test that a small enough `struct` doesn't generate unsupported pointer casts.
// (Just like `issue-373`, the problem was the use of `PassMode::Cast`, through
// the default Rust ABI adjustments, that we now override through query hooks)

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::spirv;

struct Foo {
    a: u32,
    b: u8,
    c: u8,
}

impl Foo {
    fn unpack(data: u64) -> Self {
        Self {
            a: (data >> 16 & 0xffffff) as u32,
            b: (data & 0xff >> 8) as u8,
            c: (data & 0xff) as u8,
        }
    }
}

#[spirv(fragment)]
pub fn main(#[spirv(flat)] in_packed: u64, out_sum: &mut u32) {
    let foo = Foo::unpack(in_packed);
    *out_sum = foo.a + (foo.b + foo.c) as u32;
}
