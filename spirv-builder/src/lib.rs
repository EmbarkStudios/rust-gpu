mod depfile;

use raw_string::{RawStr, RawString};
use serde::Deserialize;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::path::Path;
use std::process::{Command, Stdio};

#[derive(Debug)]
pub enum SpirvBuilderError {
    BuildFailed,
}

impl fmt::Display for SpirvBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpirvBuilderError::BuildFailed => f.write_str("Build failed"),
        }
    }
}

impl Error for SpirvBuilderError {}

pub fn build_spirv(path_to_crate: impl AsRef<Path>) -> Result<(), SpirvBuilderError> {
    let spirv_module = invoke_rustc(path_to_crate.as_ref())?;
    let spirv_path: &Path = spirv_module.as_ref();
    let env_var = spirv_path.file_name().unwrap().to_str().unwrap();
    println!("cargo:rustc-env={}={}", env_var, spirv_module);
    Ok(())
}

fn invoke_rustc(path_to_crate: &Path) -> Result<String, SpirvBuilderError> {
    // Okay, this is a little bonkers: in a normal world, we'd have the user clone
    // rustc_codegen_spirv and pass in the path to it, and then we'd invoke cargo to build it, grab
    // the resulting .so, and pass it into -Z codegen-backend. But that's really gross: the user
    // needs to clone rustc_codegen_spirv and tell us its path! So instead, we *directly reference
    // rustc_codegen_spirv in spirv-builder's Cargo.toml*, which means that it will get built
    // alongside build.rs, and cargo will helpfully add it to LD_LIBRARY_PATH for us! So we just
    // need to pass the direct filename here.
    let rustflags = format!(
        "-Z codegen-backend={}rustc_codegen_spirv{} -C target-feature=+shader",
        std::env::consts::DLL_PREFIX,
        std::env::consts::DLL_SUFFIX
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
        .stderr(Stdio::inherit())
        .current_dir(path_to_crate)
        .env("RUSTFLAGS", rustflags)
        .env("SPIRV_VAL", "1")
        .output()
        .expect("failed to execute cargo build");
    if build.status.success() {
        let stdout = String::from_utf8(build.stdout).unwrap();
        let artifact = get_last_artifact(&stdout);
        print_deps_of(&artifact);
        Ok(artifact)
    } else {
        Err(SpirvBuilderError::BuildFailed)
    }
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

    let mut filenames = last
        .filenames
        .unwrap()
        .into_iter()
        .filter(|v| v.ends_with(".spv"));
    let filename = filenames.next().expect("Crate had no .spv artifacts");
    assert_eq!(filenames.next(), None, "Crate had multiple .spv artifacts");
    filename
}

fn print_deps_of(artifact: &str) {
    let deps_file = Path::new(artifact).with_extension("d");
    let mut deps_map = HashMap::new();
    depfile::read_deps_file(&deps_file, |item, deps| {
        deps_map.insert(item, deps);
        Ok(())
    })
    .expect("Could not read dep file");
    fn recurse(map: &HashMap<RawString, Vec<RawString>>, artifact: &RawStr) {
        match map.get(artifact) {
            Some(entries) => {
                for entry in entries {
                    recurse(map, entry)
                }
            }
            None => println!("cargo:rerun-if-changed={}", artifact),
        }
    }
    recurse(&deps_map, artifact.into());
}
