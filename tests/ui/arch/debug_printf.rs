// build-pass
// compile-flags: -Ctarget-feature=+ext:SPV_KHR_non_semantic_info

use spirv_std::{
    glam::Vec2,
    macros::{debug_printf, debug_printfln},
};


fn func(a: f32, b: f32) -> f32 {
    a * b + 1.0
}

struct Struct {
    a: f32
}

impl Struct {
    fn method(&self, b: f32, c: f32) -> f32 {
        self.a * b + c
    }
}

#[spirv(fragment)]
pub fn main() {
    unsafe {
        debug_printf!();
        debug_printfln!();
        debug_printfln!("Hello World");
        debug_printfln!("Hello World",);
        debug_printfln!(r#"Hello "World""#);
    }

    let vec = Vec2::new(1.52, 25.1);

    unsafe {
        debug_printfln!("%v2f", vec);
        debug_printfln!("%v2f", { vec * 2.0 });
        debug_printfln!("%v2f", vec * 3.0);
        debug_printfln!("%% %v2f %%", vec * 4.0);
        debug_printfln!("%u %i %f 🐉", 11_u32, -11_i32, 11.0_f32);
        debug_printfln!("%f", func(33.0, 44.0));
        debug_printfln!("%f", Struct { a: 33.0 }.method(44.0, 55.0));
    }
}
