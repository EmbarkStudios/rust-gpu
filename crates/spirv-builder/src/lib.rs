//! `spirv-builder` is crate designed to automate the process of compiling and
//! building SPIR-V crates.
//!
//! # Dependencies
//! `spirv-builder` requires `cargo` to be available in the environment, in
//! order to compile crates. You can override which `cargo` is used with the
//! `CARGO` environment variable.
//!
//! # Build script example
//! In this example we'll show how to compile a SPIR-V crate to be included
//! at compile time.
//!
//! ## `build.rs`
//! ```no_run
//! use spirv_builder::{options::{Source, SourceKind}, SpirvBuilder};
//!
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//! #   let path_to_crate_source = std::path::PathBuf::new();
//!     let out_dir = std::path::PathBuf::from(std::env::var("OUT_DIR")?);
//!     SpirvBuilder::new(path_to_crate_source, &out_dir)
//!         // Currently `librustc_codegen_spirv` and the `spirv-unknown-unknown`
//!         // sysroot are not packaged with Rust, so we have to build it ourselves.
//!         // See `Source` for more information on different ways to provide or
//!         // compile the sources.
//!         .codegen_source(Source {
//!             compile_source: true,
//!             kind: SourceKind::Git {
//!                 repository: "https://github.com/EmbarkStudios/rust-gpu".into(),
//!                 into: out_dir.join("rust-gpu"),
//!                 commitish: None,
//!             }
//!         })
//!         .sysroot_location(Source {
//!             compile_source: true,
//!             kind: SourceKind::Path(out_dir.join("sysroot"))
//!         })
//!         // Emit cargo build script metadata for the SPIR-V project.
//!         .emit_build_script_metadata()
//!         .build()?;
//!     Ok(())
//! }
//! ```
//!
//! ## `main.rs`
//! ```no_compile
//! const SPIRV_BINARY: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/crate_name.spv"));
//! ```

#[cfg(test)]
mod test;

mod depfile;
mod error;
pub mod options;
mod rustc;
mod utils;

use std::env;
use std::path::{Path, PathBuf};
use std::process::Stdio;

use eyre::Result;

use options::{CodegenBuildOptions, MemoryModel, Source, Version};

pub use error::SpirvBuilderError;

/// The builder struct that compiles SPIR-V crates using
/// either a compiled or installed `librustc_codegen_spirv` and
/// `spirv-unknown-unknown` sysroot.
#[derive(Default)]
pub struct SpirvBuilder {
    build_script_metadata: bool,
    codegen_options: CodegenBuildOptions,
    codegen_source: Source,
    destination: PathBuf,
    memory_model: Option<MemoryModel>,
    rust_src: Option<PathBuf>,
    sysroot_location: Source,
    source: PathBuf,
    spirv_version: Option<Version>,
}

impl SpirvBuilder {
    /// Initialise a new [`SpirvBuilder`] with a source and
    /// destination directories.
    pub fn new(source: impl Into<PathBuf>, destination: impl Into<PathBuf>) -> Self {
        Self {
            source: source.into(),
            destination: destination.into(),
            ..Default::default()
        }
    }

    /// Sets the source for code generation.
    pub fn codegen_source(mut self, codegen_source: Source) -> Self {
        self.codegen_source = codegen_source;
        self
    }

    /// Sets the location for the sysroot.
    pub fn sysroot_location(mut self, sysroot_location: Source) -> Self {
        self.sysroot_location = sysroot_location;
        self
    }

    /// Sets the SPIR-V binary version to use. Defaults to v1.3.
    pub fn spirv_version(mut self, major: u8, minor: u8) -> Self {
        self.spirv_version = Some(Version { major, minor });
        self
    }

    /// Sets the path to the rust source to use for building sysroot. Default:
    /// The current toolchain's `rust-src` component.
    pub fn rust_src(mut self, rust_src: impl Into<PathBuf>) -> Self {
        self.rust_src = Some(rust_src.into());
        self
    }

    /// Sets the SPIR-V memory model. Defaults to Vulkan.
    pub fn memory_model(mut self, memory_model: MemoryModel) -> Self {
        self.memory_model = Some(memory_model);
        self
    }

    /// Sets whether to print build script metadata (e.g. `rerun-if-changed`
    /// for artifacts) to `cargo`.
    pub fn emit_build_script_metadata(mut self) -> Self {
        self.build_script_metadata = true;
        self
    }

    /// Creates the target feature flag for rustc, setting the SPIR-V version
    /// and/or memory model.
    fn target_feature_flag(&self) -> String {
        let mut target_features = Vec::new();
        if let Some(version) = self.spirv_version {
            target_features.push(format!("+{}", version));
        }
        if let Some(memory_model) = &self.memory_model {
            target_features.push(format!("+{}", memory_model));
        }

        if target_features.is_empty() {
            String::new()
        } else {
            format!("-C target-feature={}", target_features.join(","))
        }
    }

    fn rustc_flags(&self, sysroot: Option<impl AsRef<Path>>, codegen: impl AsRef<Path>) -> String {
        vec![
            match sysroot {
                Some(path) => format!("--sysroot={}", path.as_ref().display()),
                None => String::new(),
            },
            format!("-Zcodegen-backend={}", codegen.as_ref().display()),
            self.target_feature_flag(),
        ]
        .join(" ")
    }

    /// Builds the module. Returns the path to the built spir-v file(s).
    pub fn build(mut self) -> Result<PathBuf> {
        if std::fs::metadata(&self.destination).is_ok() {
            remove_dir_all::remove_dir_all(&self.destination)?;
        }
        std::fs::create_dir_all(&self.destination)?;
        self.destination = self.destination.canonicalize()?;
        self.source = self.source.canonicalize()?;
        utils::fix_canon_paths(&mut self.destination);
        utils::fix_canon_paths(&mut self.source);

        for module in self.build_shader()? {
            std::fs::copy(&module, self.destination.join(module.file_name().unwrap()))?;
        }

        Ok(self.destination)
    }

    /// Builds the SPIR-V codegen backend and returns a path to the
    /// compiled dynamic library. If `codegen_source.compile_source` is `false`
    /// then then it will return the `codegen_source` path with the DLL file name.
    pub fn build_spirv_codegen(&self) -> Result<PathBuf> {
        const CRATE_NAME: &str = "rustc_codegen_spirv";
        let dll_file_name = format!(
            "{}{}{}",
            env::consts::DLL_PREFIX,
            CRATE_NAME,
            env::consts::DLL_SUFFIX
        );

        let path = self.codegen_source.get()?;
        let repo_dir = match self.codegen_source.compile_source {
            true => path,
            false => return Ok(path.join(&dll_file_name)),
        };

        let crate_path = repo_dir.join("crates").join(CRATE_NAME);
        let mut args = vec![
            "build".into(),
            "--no-default-features".into(),
            format!("--features={}", self.codegen_options.spirv_tools),
            format!(
                "--manifest-path={}",
                crate_path.join("Cargo.toml").display()
            ),
        ];

        if self.codegen_options.release {
            args.push("--release".into());
        }

        let output = rustc::cargo()
            .args(&args)
            .stderr(Stdio::inherit())
            .current_dir(&crate_path)
            .output()?;

        let backend = repo_dir
            .join("target")
            .join(if self.codegen_options.release {
                "release"
            } else {
                "debug"
            })
            .join(&dll_file_name);

        if !output.status.success() || std::fs::metadata(&backend).is_err() {
            Err(eyre::eyre!("Building `rustc_codegen_spirv` failed."))
        } else {
            Ok(backend)
        }
    }

    pub fn build_sysroot(&self, backend: impl AsRef<Path>) -> Result<Option<PathBuf>> {
        const TARGET_NAME: &str = "spirv-unknown-unknown";
        if self.sysroot_location.kind.is_environment() {
            return Ok(None);
        }

        let backend = backend.as_ref();
        let path = self.sysroot_location.get()?;

        let sysroot_dir = match self.sysroot_location.compile_source {
            true => path,
            false => return Ok(Some(path)),
        };

        let mut builder =
            cargo_sysroot::SysrootBuilder::new(cargo_sysroot::Sysroot::CompilerBuiltins);
        builder
            .target(TARGET_NAME.into())
            .output(sysroot_dir)
            .rustc_flags(&[format!("-Zcodegen-backend={}", backend.display())]);

        if let Some(path) = &self.rust_src {
            builder.rust_src(path.clone());
        }

        let sysroot = builder.build().map_err(|e| eyre::eyre!("{}", e))?;
        let sysroot_codegen = sysroot.join(backend.file_name().unwrap());
        std::fs::copy(&backend, &sysroot_codegen)?;
        Ok(Some(sysroot))
    }

    pub fn build_shader(&self) -> eyre::Result<Vec<PathBuf>> {
        let backend = self.build_spirv_codegen()?;
        let sysroot = self.build_sysroot(&backend)?;
        let flags = self.rustc_flags(sysroot, backend);
        let build = rustc::cargo()
            .args(&[
                "build",
                "--verbose",
                "--release",
                "--message-format=json-render-diagnostics",
                "--target",
                "spirv-unknown-unknown",
            ])
            .stderr(Stdio::inherit())
            .current_dir(&self.source)
            .env("RUSTFLAGS", flags)
            .output()
            .expect("failed to execute cargo build");

        // get_artifacts_from_output` has the side-effect of printing invalid
        // lines, so we do that even in case of an error, to let through any
        // useful messages that ended up on stdout instead of stderr.
        let stdout = String::from_utf8(build.stdout).unwrap();
        let artifacts = rustc::get_artifacts_from_output(&stdout);

        if build.status.success() {
            if self.build_script_metadata {
                for artifact in &artifacts {
                    rustc::print_deps_of(&artifact);
                }
            }

            Ok(artifacts)
        } else {
            Err(SpirvBuilderError.into())
        }
    }
}
