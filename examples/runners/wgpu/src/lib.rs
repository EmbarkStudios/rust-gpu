// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
//#![deny(unsafe_code)]  // quite a bit of unsafe with wgpu still, would be nice to remove
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
#![allow()]

use structopt::StructOpt;
use strum::{Display, EnumString};

mod compute;
mod graphics;

#[derive(EnumString, Display, PartialEq, Copy, Clone)]
pub enum RustGPUShader {
    Simplest,
    Sky,
    Compute,
    Mouse,
}

fn maybe_watch(
    shader: RustGPUShader,
    on_watch: Option<Box<dyn FnMut(wgpu::ShaderModuleDescriptorSpirV<'static>) + Send + 'static>>,
) -> wgpu::ShaderModuleDescriptorSpirV<'static> {
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        use spirv_builder::{Capability, CompileResult, MetadataPrintout, SpirvBuilder};
        use std::borrow::Cow;
        use std::path::PathBuf;
        // Hack: spirv_builder builds into a custom directory if running under cargo, to not
        // deadlock, and the default target directory if not. However, packages like `proc-macro2`
        // have different configurations when being built here vs. when building
        // rustc_codegen_spirv normally, so we *want* to build into a separate target directory, to
        // not have to rebuild half the crate graph every time we run. So, pretend we're running
        // under cargo by setting these environment variables.
        std::env::set_var("OUT_DIR", env!("OUT_DIR"));
        std::env::set_var("PROFILE", env!("PROFILE"));
        let (crate_name, capabilities): (_, &[Capability]) = match shader {
            RustGPUShader::Simplest => ("simplest-shader", &[]),
            RustGPUShader::Sky => ("sky-shader", &[]),
            RustGPUShader::Compute => ("compute-shader", &[Capability::Int8]),
            RustGPUShader::Mouse => ("mouse-shader", &[]),
        };
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let crate_path = [manifest_dir, "..", "..", "shaders", crate_name]
            .iter()
            .copied()
            .collect::<PathBuf>();
        let mut builder = SpirvBuilder::new(crate_path, "spirv-unknown-vulkan1.1")
            .print_metadata(MetadataPrintout::None);
        for &cap in capabilities {
            builder = builder.capability(cap);
        }
        let initial_result = if let Some(mut f) = on_watch {
            builder
                .watch(move |compile_result| f(handle_compile_result(compile_result)))
                .expect("Configuration is correct for watching")
        } else {
            builder.build().unwrap()
        };
        fn handle_compile_result(
            compile_result: CompileResult,
        ) -> wgpu::ShaderModuleDescriptorSpirV<'static> {
            let module_path = compile_result.module.unwrap_single();
            let data = std::fs::read(module_path).unwrap();
            let spirv = Cow::Owned(wgpu::util::make_spirv_raw(&data).into_owned());
            wgpu::ShaderModuleDescriptorSpirV {
                label: None,
                source: spirv,
            }
        }
        handle_compile_result(initial_result)
    }
    #[cfg(any(target_os = "android", target_arch = "wasm32"))]
    {
        match shader {
            RustGPUShader::Simplest => wgpu::include_spirv_raw!(env!("simplest_shader.spv")),
            RustGPUShader::Sky => wgpu::include_spirv_raw!(env!("sky_shader.spv")),
            RustGPUShader::Compute => wgpu::include_spirv_raw!(env!("compute_shader.spv")),
            RustGPUShader::Mouse => wgpu::include_spirv_raw!(env!("mouse_shader.spv")),
        }
    }
}

fn is_compute_shader(shader: RustGPUShader) -> bool {
    shader == RustGPUShader::Compute
}

#[derive(StructOpt)]
#[structopt(name = "example-runner-wgpu")]
pub struct Options {
    #[structopt(short, long, default_value = "Sky")]
    shader: RustGPUShader,
}

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    env_logger::init();
    let options: Options = Options::from_args();

    if is_compute_shader(options.shader) {
        compute::start(&options);
    } else {
        graphics::start(&options);
    }
}
