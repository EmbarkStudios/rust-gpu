#![no_std]
#![feature(register_attr, repr_simd, core_intrinsics)]
#![register_attr(spirv)]
// Our standard Clippy lints that we use in Embark projects, we opt out of a few that are not appropriate for the specific crate (yet)
#![warn(
    clippy::all,
    clippy::doc_markdown,
    clippy::dbg_macro,
    clippy::todo,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::pub_enum_variant_names,
    clippy::mem_forget,
    clippy::filter_map_next,
    clippy::needless_continue,
    clippy::needless_borrow,
    clippy::match_wildcard_for_single_variants,
    clippy::if_let_mutex,
    clippy::mismatched_target_os,
    clippy::await_holding_lock,
    clippy::match_on_vec_items,
    clippy::imprecise_flops,
    //clippy::suboptimal_flops,
    clippy::lossy_float_literal,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::fn_params_excessive_bools,
    clippy::exit,
    clippy::inefficient_to_string,
    clippy::linkedlist,
    clippy::macro_use_imports,
    clippy::option_option,
    clippy::verbose_file_reads,
    clippy::unnested_or_patterns,
    rust_2018_idioms,
    future_incompatible,
    nonstandard_style
)]

mod math_ext;
pub use math_ext::MathExt;

pub use glam;

macro_rules! pointer_addrspace_write {
    (false) => {};
    (true) => {
        #[inline]
        #[allow(unused_attributes)]
        #[spirv(really_unsafe_ignore_bitcasts)]
        pub fn store(&mut self, v: T) {
            *self.x = v
        }
    };
}

macro_rules! pointer_addrspace {
    ($storage_class:ident, $type_name:ident, $writeable:tt) => {
        #[allow(unused_attributes)]
        #[spirv($storage_class)]
        pub struct $type_name<'a, T> {
            x: &'a mut T,
        }

        impl<'a, T: Copy> $type_name<'a, T> {
            #[inline]
            #[allow(unused_attributes)]
            #[spirv(really_unsafe_ignore_bitcasts)]
            pub fn load(&self) -> T {
                *self.x
            }

            pointer_addrspace_write!($writeable);
        }
    };
}

// Make sure these strings stay synced with symbols.rs
// Note the type names don't have to match anything, they can be renamed (only the string must match)
pointer_addrspace!(uniform_constant, UniformConstant, false);
pointer_addrspace!(input, Input, false);
pointer_addrspace!(uniform, Uniform, true);
pointer_addrspace!(output, Output, true);
pointer_addrspace!(workgroup, Workgroup, true);
pointer_addrspace!(cross_workgroup, CrossWorkgroup, true);
pointer_addrspace!(private, Private, true);
pointer_addrspace!(function, Function, true);
pointer_addrspace!(generic, Generic, true);
pointer_addrspace!(push_constant, PushConstant, false);
pointer_addrspace!(atomic_counter, AtomicCounter, true);
pointer_addrspace!(image, Image, true);
pointer_addrspace!(storage_buffer, StorageBuffer, true);
pointer_addrspace!(callable_data_khr, CallableDataKHR, true);
pointer_addrspace!(incoming_callable_data_khr, IncomingCallableDataKHR, true);
pointer_addrspace!(ray_payload_khr, RayPayloadKHR, true);
pointer_addrspace!(hit_attribute_khr, HitAttributeKHR, true);
pointer_addrspace!(incoming_ray_payload_khr, IncomingRayPayloadKHR, true);
pointer_addrspace!(shader_record_buffer_khr, ShaderRecordBufferKHR, true);
pointer_addrspace!(physical_storage_buffer, PhysicalStorageBuffer, true);

/// libcore requires a few external symbols to be defined:
/// <https://github.com/rust-lang/rust/blob/c2bc344eb23d8c1d18e803b3f1e631cf99926fbb/library/core/src/lib.rs#L23-L27>
/// TODO: This is copied from `compiler_builtins/mem.rs`. Can we use that one instead? The note in the above link says
/// "[the symbols] can also be provided by the compiler-builtins crate". The memcpy in `compiler_builtins` is behind a
/// "mem" feature flag - can we enable that somehow?
/// <https://github.com/rust-lang/compiler-builtins/blob/eff506cd49b637f1ab5931625a33cef7e91fbbf6/src/mem.rs#L12-L13>
#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32 {
    let mut i = 0;
    while i < n {
        let a = *s1.add(i);
        let b = *s2.add(i);
        if a != b {
            return a as i32 - b as i32;
        }
        i += 1;
    }
    0
}
