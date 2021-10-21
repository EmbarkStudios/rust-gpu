// build-fail
// compile-flags: -Ctarget-feature=+ext:SPV_KHR_non_semantic_info

use spirv_std::{
    macros::debug_printf,
    glam::Vec2,
};

#[spirv(fragment)]
pub fn main() {
    unsafe {
        debug_printf!("%r", 11_i32);
        debug_printf!("%f", 11_u32);
        debug_printf!("%u", 11.0_f32);
        debug_printf!("%v2f", 11.0);
        debug_printf!("%f", Vec2::splat(33.3));
    }
}
