mod depfile;

use raw_string::{RawStr, RawString};
use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};
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

pub enum MemoryModel {
    Simple,
    Vulkan,
    GLSL450,
}

pub struct SpirvBuilder {
    path_to_crate: PathBuf,
    print_metadata: bool,
    spirv_version: Option<(u8, u8)>,
    memory_model: Option<MemoryModel>,
}
impl SpirvBuilder {
    pub fn new(path_to_crate: impl AsRef<Path>) -> Self {
        Self {
            path_to_crate: path_to_crate.as_ref().to_owned(),
            print_metadata: true,
            spirv_version: None,
            memory_model: None,
        }
    }

    /// Whether to print build.rs cargo metadata (e.g. cargo:rustc-env=var=val). Defaults to true.
    pub fn print_metadata(mut self, v: bool) -> Self {
        self.print_metadata = v;
        self
    }

    /// Sets the SPIR-V binary version to use. Defaults to v1.3.
    pub fn spirv_version(mut self, major: u8, minor: u8) -> Self {
        self.spirv_version = Some((major, minor));
        self
    }

    /// Sets the SPIR-V memory model. Defaults to Vulkan.
    pub fn memory_model(mut self, memory_model: MemoryModel) -> Self {
        self.memory_model = Some(memory_model);
        self
    }

    /// Builds the module. Returns the path to the built spir-v file. If print_metadata is true,
    /// you usually don't have to inspect the path, as the environment variable will already be
    /// set.
    pub fn build(self) -> Result<PathBuf, SpirvBuilderError> {
        let spirv_module = invoke_rustc(&self)?;
        let env_var = spirv_module.file_name().unwrap().to_str().unwrap();
        if self.print_metadata {
            println!("cargo:rustc-env={}={}", env_var, spirv_module.display());
        }
        Ok(spirv_module)
    }
}

// https://github.com/rust-lang/cargo/blob/1857880b5124580c4aeb4e8bc5f1198f491d61b1/src/cargo/util/paths.rs#L29-L52
fn dylib_path_envvar() -> &'static str {
    if cfg!(windows) {
        "PATH"
    } else if cfg!(target_os = "macos") {
        "DYLD_FALLBACK_LIBRARY_PATH"
    } else {
        "LD_LIBRARY_PATH"
    }
}
fn dylib_path() -> Vec<PathBuf> {
    match env::var_os(dylib_path_envvar()) {
        Some(var) => env::split_paths(&var).collect(),
        None => Vec::new(),
    }
}

fn find_rustc_codegen_spirv() -> PathBuf {
    let filename = format!(
        "{}rustc_codegen_spirv{}",
        env::consts::DLL_PREFIX,
        env::consts::DLL_SUFFIX
    );
    for mut path in dylib_path() {
        path.push(&filename);
        if path.is_file() {
            return path;
        }
    }
    panic!("Could not find {} in library path", filename);
}

fn invoke_rustc(builder: &SpirvBuilder) -> Result<PathBuf, SpirvBuilderError> {
    // Okay, this is a little bonkers: in a normal world, we'd have the user clone
    // rustc_codegen_spirv and pass in the path to it, and then we'd invoke cargo to build it, grab
    // the resulting .so, and pass it into -Z codegen-backend. But that's really gross: the user
    // needs to clone rustc_codegen_spirv and tell us its path! So instead, we *directly reference
    // rustc_codegen_spirv in spirv-builder's Cargo.toml*, which means that it will get built
    // alongside build.rs, and cargo will helpfully add it to LD_LIBRARY_PATH for us! However,
    // rustc expects a full path, instead of a filename looked up via LD_LIBRARY_PATH, so we need
    // to copy cargo's understanding of library lookup and find the library and its full path.
    let rustc_codegen_spirv = find_rustc_codegen_spirv();
    let mut target_features = Vec::new();
    // these must match codegen_cx/mod.rs
    if let Some((major, minor)) = builder.spirv_version {
        target_features.push(format!("+spirv{}.{}", major, minor));
    }
    if let Some(memory_model) = &builder.memory_model {
        target_features.push(
            match memory_model {
                MemoryModel::Simple => "+simple",
                MemoryModel::Vulkan => "+vulkan",
                MemoryModel::GLSL450 => "+glsl450",
            }
            .to_string(),
        );
    }
    let feature_flag = if target_features.is_empty() {
        String::new()
    } else {
        format!(" -C target-feature={}", target_features.join(","))
    };
    let rustflags = format!(
        "-Z codegen-backend={}{}",
        rustc_codegen_spirv.display(),
        feature_flag,
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
        .current_dir(&builder.path_to_crate)
        .env("RUSTFLAGS", rustflags)
        .output()
        .expect("failed to execute cargo build");
    if build.status.success() {
        let stdout = String::from_utf8(build.stdout).unwrap();
        let artifact = get_last_artifact(&stdout);
        if builder.print_metadata {
            print_deps_of(&artifact);
        }
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

fn get_last_artifact(out: &str) -> PathBuf {
    let last = out
        .lines()
        .filter_map(|line| match serde_json::from_str::<RustcOutput>(line) {
            Ok(line) => Some(line),
            Err(_) => {
                // Pass through invalid lines
                println!("{}", line);
                None
            }
        })
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
    filename.into()
}

fn print_deps_of(artifact: &Path) {
    let deps_file = artifact.with_extension("d");
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
    recurse(&deps_map, artifact.to_str().unwrap().into());
}
