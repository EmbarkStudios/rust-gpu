use serde::Deserialize;
use std::error::Error;
use std::fmt;
use std::path::Path;
use std::process::{Command, Output};

#[derive(Debug)]
pub enum SpirvBuilderError {
    BuildFailed(Output),
}

impl fmt::Display for SpirvBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpirvBuilderError::BuildFailed(output) => write!(
                f,
                "{}\nstdout:\n{}\nstderr:\n{}",
                output.status,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ),
        }
    }
}

impl Error for SpirvBuilderError {}

pub fn build_spirv(
    path_to_rustc_codegen_spirv: impl AsRef<Path>,
    path_to_crate: impl AsRef<Path>,
) -> Result<(), SpirvBuilderError> {
    let path_to_built_codegen = build_rustc_codegen_spirv(path_to_rustc_codegen_spirv.as_ref())?;
    let spirv_module = invoke_rustc(path_to_built_codegen, path_to_crate.as_ref())?;
    let spirv_path: &Path = spirv_module.as_ref();
    let env_var = spirv_path.file_name().unwrap().to_str().unwrap();
    println!("cargo:rustc-env={}={}", env_var, spirv_module);
    Ok(())
}

#[derive(Deserialize)]
struct RustcOutput {
    reason: String,
    filenames: Option<Vec<String>>,
}

fn get_last_artifact(out: &str) -> String {
    let out = serde_json::Deserializer::from_str(out).into_iter::<RustcOutput>();
    let last = out
        .map(|line| line.unwrap())
        .filter(|line| line.reason == "compiler-artifact")
        .last()
        .expect("Did not find output file in rustc output");

    let mut filenames = last.filenames.unwrap();
    eprintln!("{:?}", filenames);
    if cfg!(windows) {
        filenames
            .iter()
            .find(|&v| v.ends_with(".dll") || v.ends_with(".spv"))
            .unwrap()
            .clone()
    } else {
        assert_eq!(filenames.len(), 1);
        filenames.pop().unwrap()
    }
}

fn build_rustc_codegen_spirv(path_to_codegen: &Path) -> Result<String, SpirvBuilderError> {
    let build = Command::new("cargo")
        .args(&[
            "build",
            "--message-format=json-render-diagnostics",
            "--release",
        ])
        .current_dir(path_to_codegen)
        .output()
        .expect("failed to execute cargo build");
    if build.status.success() {
        Ok(get_last_artifact(&String::from_utf8(build.stdout).unwrap()))
    } else {
        Err(SpirvBuilderError::BuildFailed(build))
    }
}

fn invoke_rustc(
    path_to_built_codegen: String,
    path_to_crate: &Path,
) -> Result<String, SpirvBuilderError> {
    let rustflags = format!(
        "-Z codegen-backend={} -C target-feature=+shader",
        path_to_built_codegen
    );
    let build = Command::new("cargo")
        .args(&[
            "build",
            "--message-format=json-render-diagnostics",
            "-Z",
            "build-std=core",
            "--target",
            "spirv-unknown-unknown",
            "--release",
        ])
        .current_dir(path_to_crate)
        .env("RUSTFLAGS", rustflags)
        .env("SPIRV_VAL", "1")
        .output()
        .expect("failed to execute cargo build");
    if build.status.success() {
        Ok(get_last_artifact(&String::from_utf8(build.stdout).unwrap()))
    } else {
        Err(SpirvBuilderError::BuildFailed(build))
    }
}
