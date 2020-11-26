use clap::Clap;
use strum::{Display, EnumString};

mod compute;
mod graphics;

#[derive(EnumString, Display, PartialEq, Copy, Clone)]
pub enum RustGPUShader {
    Simplest,
    Sky,
    Compute,
}

fn shader_module(shader: RustGPUShader) -> wgpu::ShaderModuleSource<'static> {
    match shader {
        RustGPUShader::Compute => {
            wgpu::include_spirv!(concat!(env!("OUT_DIR"), "/compute_shader.spv"))
        }
        RustGPUShader::Simplest => {
            wgpu::include_spirv!(concat!(env!("OUT_DIR"), "/simplest_shader.spv"))
        }
        RustGPUShader::Sky => {
            wgpu::include_spirv!(concat!(env!("OUT_DIR"), "/sky_shader.spv"))
        }
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
