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
    let path_to_src = match shader {
        RustGPUShader::Simplest => "examples/shaders/simplest-shader",
        RustGPUShader::Sky => "examples/shaders/sky-shader",
        RustGPUShader::Compute => "examples/shaders/compute-shader",
        RustGPUShader::Mouse => "examples/shaders/mouse-shader",
    };

    // rustc_codegen_spirv gets built by the spirv-builder build dependency
    // the resulting dll needs to be retrieved directly or IC won't work
    let rustc_codegen_spirv = {
        let dll = format!(
            "{}rustc_codegen_spirv{}",
            std::env::consts::DLL_PREFIX,
            std::env::consts::DLL_SUFFIX,
        );
        let release = std::path::PathBuf::from("target/release").join(&dll);
        let debug = std::path::PathBuf::from("target/debug").join(&dll);
        if release.exists() {
            release.canonicalize().unwrap()
        } else if debug.exists() {
            debug.canonicalize().unwrap()
        } else {
            // cargo will search a series of directories (including the rustup tools directory),
            // but IC won't work unless it's located in the tools directory.
            std::path::PathBuf::from(dll)
        }
    };

    std::process::Command::new("cargo")
        .stderr(std::process::Stdio::inherit())
        .env("CARGO_INCREMENTAL", "1")
        .args(&[
            "build",
            "-Z",
            "build-std=core",
            "--target",
            "spirv-unknown-unknown",
            "--release",
            "--manifest-path",
            &format!("{}/Cargo.toml", path_to_src),
        ])
        .env("RUSTFLAGS", format!(
            "-Z codegen-backend={} -Z symbol-mangling-version=v0 -C target-feature=+spirv1.0,+vulkan",
            rustc_codegen_spirv.display(),
        ))
        .output()
        .expect("failed to execute cargo build");

    std::fs::read(&format!(
        "target/spirv-unknown-unknown/release/{}.spv",
        path_to_src.split('/').last().unwrap().replace("-", "_")
    ))
    .expect("failed to read .spv file")
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
