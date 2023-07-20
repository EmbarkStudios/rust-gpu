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
// #![allow()]

use std::borrow::Cow;
use structopt::StructOpt;
use strum::{Display, EnumString};

// NOTE(eddyb) while this could theoretically work on the web, it needs more work.
#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
mod compute;

mod graphics;

#[derive(EnumString, Display, PartialEq, Eq, Copy, Clone)]
pub enum RustGPUShader {
    Simplest,
    Sky,
    Compute,
    Mouse,
}

struct CompiledShaderModules {
    named_spv_modules: Vec<(Option<String>, wgpu::ShaderModuleDescriptorSpirV<'static>)>,
}

impl CompiledShaderModules {
    fn spv_module_for_entry_point<'a>(
        &'a self,
        wanted_entry: &str,
    ) -> wgpu::ShaderModuleDescriptorSpirV<'a> {
        for (name, spv_module) in &self.named_spv_modules {
            match name {
                Some(name) if name != wanted_entry => continue,
                _ => {
                    return wgpu::ShaderModuleDescriptorSpirV {
                        label: name.as_deref(),
                        source: Cow::Borrowed(&spv_module.source),
                    };
                }
            }
        }
        unreachable!(
            "{wanted_entry:?} not found in modules {:?}",
            self.named_spv_modules
                .iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>()
        );
    }
}

fn maybe_watch(
    options: &Options,
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))] on_watch: Option<
        Box<dyn FnMut(CompiledShaderModules) + Send + 'static>,
    >,
) -> CompiledShaderModules {
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        use spirv_builder::{CompileResult, MetadataPrintout, SpirvBuilder};
        use std::path::PathBuf;
        // Hack: spirv_builder builds into a custom directory if running under cargo, to not
        // deadlock, and the default target directory if not. However, packages like `proc-macro2`
        // have different configurations when being built here vs. when building
        // rustc_codegen_spirv normally, so we *want* to build into a separate target directory, to
        // not have to rebuild half the crate graph every time we run. So, pretend we're running
        // under cargo by setting these environment variables.
        std::env::set_var("OUT_DIR", env!("OUT_DIR"));
        std::env::set_var("PROFILE", env!("PROFILE"));
        let crate_name = match options.shader {
            RustGPUShader::Simplest => "simplest-shader",
            RustGPUShader::Sky => "sky-shader",
            RustGPUShader::Compute => "compute-shader",
            RustGPUShader::Mouse => "mouse-shader",
        };
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let crate_path = [manifest_dir, "..", "..", "shaders", crate_name]
            .iter()
            .copied()
            .collect::<PathBuf>();

        let has_debug_printf = options.force_spirv_passthru;

        let builder = SpirvBuilder::new(crate_path, "spirv-unknown-vulkan1.1")
            .print_metadata(MetadataPrintout::None)
            .shader_panic_strategy(if has_debug_printf {
                spirv_builder::ShaderPanicStrategy::DebugPrintfThenExit {
                    print_inputs: true,
                    print_backtrace: true,
                }
            } else {
                spirv_builder::ShaderPanicStrategy::SilentExit
            })
            // HACK(eddyb) needed because of `debugPrintf` instrumentation limitations
            // (see https://github.com/KhronosGroup/SPIRV-Tools/issues/4892).
            .multimodule(has_debug_printf);
        let initial_result = if let Some(mut f) = on_watch {
            builder
                .watch(move |compile_result| f(handle_compile_result(compile_result)))
                .expect("Configuration is correct for watching")
        } else {
            builder.build().unwrap()
        };
        fn handle_compile_result(compile_result: CompileResult) -> CompiledShaderModules {
            let load_spv_module = |path| {
                let data = std::fs::read(path).unwrap();
                // FIXME(eddyb) this reallocates all the data pointlessly, there is
                // not a good reason to use `ShaderModuleDescriptorSpirV` specifically.
                let spirv = Cow::Owned(wgpu::util::make_spirv_raw(&data).into_owned());
                wgpu::ShaderModuleDescriptorSpirV {
                    label: None,
                    source: spirv,
                }
            };
            CompiledShaderModules {
                named_spv_modules: match compile_result.module {
                    spirv_builder::ModuleResult::SingleModule(path) => {
                        vec![(None, load_spv_module(path))]
                    }
                    spirv_builder::ModuleResult::MultiModule(modules) => modules
                        .into_iter()
                        .map(|(name, path)| (Some(name), load_spv_module(path)))
                        .collect(),
                },
            }
        }
        handle_compile_result(initial_result)
    }
    #[cfg(any(target_os = "android", target_arch = "wasm32"))]
    {
        let module = match options.shader {
            RustGPUShader::Simplest => {
                wgpu::include_spirv_raw!(env!("simplest_shader.spv"))
            }
            RustGPUShader::Sky => wgpu::include_spirv_raw!(env!("sky_shader.spv")),
            RustGPUShader::Compute => wgpu::include_spirv_raw!(env!("compute_shader.spv")),
            RustGPUShader::Mouse => wgpu::include_spirv_raw!(env!("mouse_shader.spv")),
        };
        CompiledShaderModules {
            named_spv_modules: vec![(None, module)],
        }
    }
}

#[derive(StructOpt, Clone)]
#[structopt(name = "example-runner-wgpu")]
pub struct Options {
    #[structopt(short, long, default_value = "Sky")]
    shader: RustGPUShader,

    #[structopt(long)]
    force_spirv_passthru: bool,
}

#[cfg_attr(target_os = "android", export_name = "android_main")]
pub fn main(
    #[cfg(target_os = "android")] android_app: winit::platform::android::activity::AndroidApp,
) {
    let options: Options = Options::from_args();

    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    if options.shader == RustGPUShader::Compute {
        return compute::start(&options);
    }

    graphics::start(
        #[cfg(target_os = "android")]
        android_app,
        &options,
    );
}
