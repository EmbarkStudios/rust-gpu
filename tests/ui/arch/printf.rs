// build-pass
// compile-flags: -Ctarget-feature=+ext:SPV_KHR_non_semantic_info

use spirv_std::{macros::{printf, printfln}, glam::Vec2};

#[spirv(fragment)]
pub fn main() {
    unsafe {
        printf!();
        printfln!();
        printfln!("Hello World");
    }

    let vec = Vec2::new(1.52, 25.1);

    unsafe {
        printfln!("%v2f", vec);
        printfln!("%v2f", { vec * 2.0 });
        printfln!("%v2f", vec * 3.0);
        printfln!("%% %v2f %%", vec * 4.0);
        printfln!("%u %i %f 🐉", 11_u32, -11_i32, 11.0_f32);
    }
}
