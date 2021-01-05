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

#[derive(Clap)]
pub struct Options {
    #[clap(short, long, default_value = "Sky")]
    shader: RustGPUShader,
}

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    let options: Options = Options::parse();

    if is_compute_shader(options.shader) {
        futures::executor::block_on(compute::start(&options, vec![1, 2, 3, 4]));
    } else {
        futures::executor::block_on(graphics::start(&options));
    }
}
