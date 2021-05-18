// BEGIN - Embark standard lints v0.3
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
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
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::pub_enum_variant_names,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::todo,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::verbose_file_reads,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.3
// crate-specific exceptions:
#![allow()]

mod depfile;

use raw_string::{RawStr, RawString};
use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub use rustc_codegen_spirv::{CompileResult, ModuleResult};

#[derive(Debug)]
pub enum SpirvBuilderError {
    CratePathDoesntExist(PathBuf),
    BuildFailed,
    MultiModuleWithPrintMetadata,
    MetadataFileMissing(std::io::Error),
    MetadataFileMalformed(serde_json::Error),
}

impl fmt::Display for SpirvBuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpirvBuilderError::CratePathDoesntExist(path) => {
                write!(f, "Crate path {} does not exist", path.display())
            }
            SpirvBuilderError::BuildFailed => f.write_str("Build failed"),
            SpirvBuilderError::MultiModuleWithPrintMetadata => {
                f.write_str("Multi-module build cannot be used with print_metadata = true")
            }
            SpirvBuilderError::MetadataFileMissing(_) => {
                f.write_str("Multi-module metadata file missing")
            }
            SpirvBuilderError::MetadataFileMalformed(_) => {
                f.write_str("Unable to parse multi-module metadata file")
            }
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
    release: bool,
    target: String,
    bindless: bool,
    multimodule: bool,
}

impl SpirvBuilder {
    pub fn new(path_to_crate: impl AsRef<Path>, target: impl Into<String>) -> Self {
        Self {
            path_to_crate: path_to_crate.as_ref().to_owned(),
            print_metadata: true,
            release: true,
            target: target.into(),
            bindless: false,
            multimodule: false,
        }
    }

    /// Whether to print build.rs cargo metadata (e.g. cargo:rustc-env=var=val). Defaults to true.
    pub fn print_metadata(mut self, v: bool) -> Self {
        self.print_metadata = v;
        self
    }

    /// Run the compiler in bindless mode, this flag is in preparation for the full feature
    /// and it's expected to be the default mode going forward
    pub fn bindless(mut self, v: bool) -> Self {
        self.bindless = v;
        self
    }

    /// Build in release. Defaults to true.
    pub fn release(mut self, v: bool) -> Self {
        self.release = v;
        self
    }

    /// Splits the resulting SPIR-V file into one module per entry point. This is useful in cases
    /// where ecosystem tooling has bugs around multiple entry points per module - having all entry
    /// points bundled into a single file is the preferred system.
    pub fn multimodule(mut self, v: bool) -> Self {
        self.multimodule = v;
        self
    }

    /// Builds the module. If `print_metadata` is true, you usually don't have to inspect the path
    /// in the result, as the environment variable for the path to the module will already be set.
    pub fn build(self) -> Result<CompileResult, SpirvBuilderError> {
        if self.print_metadata && self.multimodule {
            return Err(SpirvBuilderError::MultiModuleWithPrintMetadata);
        }
        if !self.path_to_crate.is_dir() {
            return Err(SpirvBuilderError::CratePathDoesntExist(self.path_to_crate));
        }
        let metadata_file = invoke_rustc(&self)?;
        let metadata_contents =
            File::open(&metadata_file).map_err(SpirvBuilderError::MetadataFileMissing)?;
        let metadata: CompileResult = serde_json::from_reader(BufReader::new(metadata_contents))
            .map_err(SpirvBuilderError::MetadataFileMalformed)?;
        match &metadata.module {
            ModuleResult::SingleModule(spirv_module) => {
                assert!(!self.multimodule);
                let env_var = metadata_file.file_name().unwrap().to_str().unwrap();
                if self.print_metadata {
                    println!("cargo:rustc-env={}={}", env_var, spirv_module.display());
                }
            }
            ModuleResult::MultiModule(_) => {
                assert!(self.multimodule);
            }
        }
        Ok(metadata)
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

// Returns path to the metadata json.
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
    let llvm_args = builder
        .multimodule
        .then(|| " -C llvm-args=--module-output=multiple")
        .unwrap_or_default();

    let mut target_features = Vec::new();

    if builder.bindless {
        target_features.push("+bindless");
    }

    let feature_flag = if target_features.is_empty() {
        String::new()
    } else {
        format!(" -C target-feature={}", target_features.join(","))
    };

    let rustflags = format!(
        "-Z codegen-backend={} -Zsymbol-mangling-version=v0{}{}",
        rustc_codegen_spirv.display(),
        feature_flag,
        llvm_args,
    );

    let mut cargo = Command::new("cargo");
    cargo.args(&[
        "build",
        "--message-format=json-render-diagnostics",
        "-Zbuild-std=core",
        "-Zbuild-std-features=compiler-builtins-mem",
        "--target",
        &*builder.target,
    ]);

    if builder.release {
        cargo.arg("--release");
    }

    // If we're nested in `cargo` invocation, use a different `--target-dir`,
    // to avoid waiting on the same lock (which effectively dead-locks us).
    // This also helps with e.g. RLS, which uses `--target target/rls`,
    // so we'll have a separate `target/rls/spirv-builder` for it.
    if let (Ok(profile), Some(mut dir)) = (
        env::var("PROFILE"),
        env::var_os("OUT_DIR").map(PathBuf::from),
    ) {
        // Strip `$profile/build/*/out`.
        if dir.ends_with("out")
            && dir.pop()
            && dir.pop()
            && dir.ends_with("build")
            && dir.pop()
            && dir.ends_with(profile)
            && dir.pop()
        {
            cargo.arg("--target-dir").arg(dir.join("spirv-builder"));
        }
    }

    let build = cargo
        .stderr(Stdio::inherit())
        .current_dir(&builder.path_to_crate)
        .env("RUSTFLAGS", rustflags)
        .output()
        .expect("failed to execute cargo build");

    // `get_last_artifact` has the side-effect of printing invalid lines, so
    // we do that even in case of an error, to let through any useful messages
    // that ended up on stdout instead of stderr.
    let stdout = String::from_utf8(build.stdout).unwrap();
    let artifact = get_last_artifact(&stdout);

    if build.status.success() {
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
