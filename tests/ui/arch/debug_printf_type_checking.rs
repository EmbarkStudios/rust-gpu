// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// compile-flags: -Ctarget-feature=+ext:SPV_KHR_non_semantic_info

use spirv_std::spirv;
use spirv_std::{glam::Vec2, macros::debug_printf};

#[spirv(fragment)]
pub fn main() {
    unsafe {
        debug_printf!("%1");
        debug_printf!("%1.");
        debug_printf!("%.");
        debug_printf!("%.1");
        debug_printf!("%1.1");
        debug_printf!("%1.1v");
        debug_printf!("%1.1v5");
        debug_printf!("%1.1v2");
        debug_printf!("%1.1v2r");
        debug_printf!("%r", 11_i32);
        debug_printf!("%f", 11_u32);
        debug_printf!("%u", 11.0_f32);
        debug_printf!("%v2f", 11.0);
        debug_printf!("%f", Vec2::splat(33.3));
    }
}
