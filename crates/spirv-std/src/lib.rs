#![no_std]
#![cfg_attr(
    target_arch = "spirv",
    feature(asm, register_attr, repr_simd, core_intrinsics, lang_items),
    register_attr(spirv)
)]
#![cfg_attr(
    feature = "const-generics",
    feature(const_generics),
    allow(incomplete_features)
)]
// BEGIN - Embark standard lints v0.3
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::explicit_into_iter_loop,
    clippy::filter_map_next,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::pub_enum_variant_names,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::todo,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::verbose_file_reads,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.3
// crate-specific exceptions:
#![allow(
    // Needed for `asm!`.
    unsafe_code,
    // We deblierately provide an unimplemented version of our API on CPU
    // platforms so that code completion still works.
    clippy::unimplemented,
)]

#[macro_use]
#[cfg(not(target_arch = "spirv"))]
pub extern crate spirv_std_macros as macros;

pub mod arch;
pub mod float;
pub mod integer;
pub mod memory;
pub mod ray_tracing;
pub mod scalar;
pub(crate) mod sealed;
mod textures;
pub mod vector;

pub use num_traits;
pub use textures::*;

/// Calls the `OpDemoteToHelperInvocationEXT` instruction, which corresponds to discard() in HLSL
#[spirv_std_macros::gpu_only]
pub fn demote_to_helper_invocation() {
    unsafe {
        asm!(
            "OpExtension \"SPV_EXT_demote_to_helper_invocation\"",
            "OpCapability DemoteToHelperInvocationEXT",
            "OpDemoteToHelperInvocationEXT"
        );
    }
}

/// Calls the `OpKill` instruction, which corresponds to discard() in GLSL
#[spirv_std_macros::gpu_only]
pub fn discard() {
    unsafe {
        asm!("OpKill", "%unused = OpLabel");
    }
}

#[cfg(all(not(test), target_arch = "spirv"))]
#[panic_handler]
fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
    loop {}
}

#[cfg(all(not(test), target_arch = "spirv"))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}
