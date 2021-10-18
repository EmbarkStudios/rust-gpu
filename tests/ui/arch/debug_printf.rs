// build-pass
// compile-flags: -Ctarget-feature=+ext:SPV_KHR_non_semantic_info

use spirv_std::{
    glam::Vec2,
    macros::{debug_printf, debug_printfln},
};

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
    }
}
