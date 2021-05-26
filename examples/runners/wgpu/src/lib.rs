// standard Embark lints
//#![deny(unsafe_code)]  // quite a bit of unsafe with wgpu still, would be nice to remove
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
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::pub_enum_variant_names,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::todo,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::verbose_file_reads,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]

use clap::Clap;
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

fn shader_module(shader: RustGPUShader) -> wgpu::ShaderModuleDescriptor<'static> {
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        use spirv_builder::{Capability, SpirvBuilder};
        use std::borrow::Cow;
        use std::path::{Path, PathBuf};
        // Hack: spirv_builder builds into a custom directory if running under cargo, to not
        // deadlock, and the default target directory if not. However, packages like `proc-macro2`
        // have different configurations when being built here vs. when building
        // rustc_codegen_spirv normally, so we *want* to build into a separate target directory, to
        // not have to rebuild half the crate graph every time we run. So, pretend we're running
        // under cargo by setting these environment variables.
        std::env::set_var("OUT_DIR", env!("OUT_DIR"));
        std::env::set_var("PROFILE", env!("PROFILE"));
        let (crate_name, capabilities): (_, &[Capability]) = match shader {
            RustGPUShader::Simplest => ("sky-shader", &[]),
            RustGPUShader::Sky => ("simplest-shader", &[]),
            RustGPUShader::Compute => ("compute-shader", &[]),
            RustGPUShader::Mouse => ("mouse-shader", &[]),
        };
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let crate_path = [
            Path::new(manifest_dir),
            Path::new(".."),
            Path::new(".."),
            Path::new("shaders"),
            Path::new(crate_name),
        ]
        .iter()
        .copied()
        .collect::<PathBuf>();
        let mut builder =
            SpirvBuilder::new(crate_path, "spirv-unknown-vulkan1.0").print_metadata(false);
        for &cap in capabilities {
            builder = builder.capability(cap);
        }
        let compile_result = builder.build().unwrap();
        let module_path = compile_result.module.unwrap_single();
        let data = std::fs::read(module_path).unwrap();
        let spirv = wgpu::util::make_spirv(&data);
        let spirv = match spirv {
            wgpu::ShaderSource::Wgsl(cow) => wgpu::ShaderSource::Wgsl(Cow::Owned(cow.into_owned())),
            wgpu::ShaderSource::SpirV(cow) => {
                wgpu::ShaderSource::SpirV(Cow::Owned(cow.into_owned()))
            }
        };
        wgpu::ShaderModuleDescriptor {
            label: None,
            source: spirv,
            flags: wgpu::ShaderFlags::default(),
        }
    }
    #[cfg(any(target_os = "android", target_arch = "wasm32"))]
    match shader {
        RustGPUShader::Simplest => wgpu::include_spirv!(env!("simplest_shader.spv")),
        RustGPUShader::Sky => wgpu::include_spirv!(env!("sky_shader.spv")),
        RustGPUShader::Compute => wgpu::include_spirv!(env!("compute_shader.spv")),
        RustGPUShader::Mouse => wgpu::include_spirv!(env!("mouse_shader.spv")),
    }
}

fn is_compute_shader(shader: RustGPUShader) -> bool {
    shader == RustGPUShader::Compute
}

#[derive(Clap)]
pub struct Options {
    #[clap(short, long, default_value = "Sky")]
    shader: RustGPUShader,
}

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    let options: Options = Options::parse();

    if is_compute_shader(options.shader) {
        compute::start(&options)
    } else {
        graphics::start(&options);
    }
}
