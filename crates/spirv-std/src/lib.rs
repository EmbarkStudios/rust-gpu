#![no_std]
#![cfg_attr(
    target_arch = "spirv",
    feature(
        asm,
        register_attr,
        repr_simd,
        core_intrinsics,
        lang_items,
        abi_unadjusted
    ),
    register_attr(spirv)
)]
// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::todo,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.4
// crate-specific exceptions:
#![allow(
    // Needed for `asm!`.
    unsafe_code,
    // We deblierately provide an unimplemented version of our API on CPU
    // platforms so that code completion still works.
    clippy::unimplemented,
    // The part of `adt_const_params` we're using (C-like enums) is not incomplete.
    incomplete_features,
)]

#[cfg_attr(not(target_arch = "spirv"), macro_use)]
pub extern crate spirv_std_macros as macros;

pub mod arch;
pub mod byte_addressable_buffer;
pub mod float;
pub mod image;
pub mod integer;
pub mod memory;
pub mod ray_tracing;
mod runtime_array;
mod sampler;
pub mod scalar;
pub(crate) mod sealed;
pub mod vector;

pub use self::sampler::Sampler;
pub use crate::macros::Image;
pub use byte_addressable_buffer::ByteAddressableBuffer;
pub use num_traits;
pub use runtime_array::*;

#[cfg(feature = "glam")]
pub use glam;

#[cfg(all(not(test), target_arch = "spirv"))]
#[panic_handler]
fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
    loop {}
}

#[cfg(all(not(test), target_arch = "spirv"))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}

// See: https://github.com/rust-lang/rust/issues/84738
#[doc(hidden)]
/// [spirv_types]
pub fn workaround_rustdoc_ice_84738() {}
