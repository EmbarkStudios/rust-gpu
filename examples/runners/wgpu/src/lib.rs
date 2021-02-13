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

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
fn compile_shader(shader: RustGPUShader) -> Vec<u8> {
    let compile_spv_module = |path_to_src: &str| {
        let spv_path = spirv_builder::SpirvBuilder::new(path_to_src)
            .spirv_version(1, 0)
            .print_metadata(false)
            .build()
            .expect("failed to compile spirv");
        std::fs::read(spv_path).expect("failed to read .spv file")
    };
    match shader {
        RustGPUShader::Simplest => compile_spv_module("examples/shaders/simplest-shader"),
        RustGPUShader::Sky => compile_spv_module("examples/shaders/sky-shader"),
        RustGPUShader::Compute => compile_spv_module("examples/shaders/compute-shader"),
        RustGPUShader::Mouse => compile_spv_module("examples/shaders/mouse-shader"),
    }
}

#[cfg(any(target_os = "android", target_arch = "wasm32"))]
fn shader_module(shader: RustGPUShader) -> wgpu::ShaderModuleSource<'static> {
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

#[derive(Clap, Copy, Clone)]
pub struct Options {
    #[clap(short, long, default_value = "Sky")]
    shader: RustGPUShader,
}

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    let options: Options = Options::parse();

    if is_compute_shader(options.shader) {
        compute::start(options)
    } else {
        graphics::start(options);
    }
}
