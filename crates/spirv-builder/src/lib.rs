// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::todo,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.4
// crate-specific exceptions:
// #![allow()]
#![doc = include_str!("../README.md")]

// HACK(eddyb) try to catch misuse of Cargo package features very early on
// (see `spirv-builder/Cargo.toml` for why we go through all of this).
#[cfg(all(
    not(any(feature = "use-compiled-tools", feature = "use-installed-tools")),
    not(doc)
))]
compile_error!(
    "at least one of `use-compiled-tools` or `use-installed-tools` features must be enabled
(outside of documentation builds, which require disabling both to build on stable)"
);

#[cfg(doc)]
fn _ensure_cfg_doc_means_rustdoc() {
    // HACK(eddyb) this relies on specific `rustdoc` behavior (i.e. it skips
    // type-checking function bodies, so we trigger a compile-time `panic! from
    // a type) to check that we're in fact under `rustdoc`, not just `--cfg doc`.
    #[rustfmt::skip]
    let _: [(); panic!("

            `--cfg doc` was set outside of `rustdoc`
            (if you are running `rustdoc` or `cargo doc`, please file an issue)

")];
}

mod depfile;
#[cfg(feature = "watch")]
mod watch;

use raw_string::{RawStr, RawString};
use serde::Deserialize;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub use rustc_codegen_spirv_types::Capability;
pub use rustc_codegen_spirv_types::{CompileResult, ModuleResult};

#[derive(Debug)]
#[non_exhaustive]
pub enum SpirvBuilderError {
    CratePathDoesntExist(PathBuf),
    BuildFailed,
    MultiModuleWithPrintMetadata,
    WatchWithPrintMetadata,
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
            SpirvBuilderError::MultiModuleWithPrintMetadata => f.write_str(
                "Multi-module build cannot be used with print_metadata = MetadataPrintout::Full",
            ),
            SpirvBuilderError::WatchWithPrintMetadata => {
                f.write_str("Watching within build scripts will prevent build completion")
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MetadataPrintout {
    /// Print no cargo metadata.
    None,
    /// Print only dependency information (eg for multiple modules).
    DependencyOnly,
    /// Print all cargo metadata.
    ///
    /// Includes dependency information and spirv environment variable.
    Full,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SpirvMetadata {
    /// Strip all names and other debug information from SPIR-V output.
    None,
    /// Only include OpNames for public interface variables (uniforms and the like), to allow
    /// shader reflection.
    NameVariables,
    /// Include all OpNames for everything, and OpLines. Significantly increases binary size.
    Full,
}

/// Strategy used to handle Rust `panic!`s in shaders compiled to SPIR-V.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ShaderPanicStrategy {
    /// Return from shader entry-point with no side-effects **(default)**.
    ///
    /// While similar to the standard SPIR-V `OpTerminateInvocation`, this is
    /// *not* limited to fragment shaders, and instead supports all shaders
    /// (as it's handled via control-flow rewriting, instead of SPIR-V features).
    SilentExit,

    /// Like `SilentExit`, but also using `debugPrintf` to report the panic in
    /// a way that can reach the user, before returning from the entry-point.
    ///
    /// Will automatically require the `SPV_KHR_non_semantic_info` extension,
    /// as `debugPrintf` uses a "non-semantic extended instruction set".
    ///
    /// If you have multiple entry-points, you *may* need to also enable the
    /// `multimodule` node (see <https://github.com/KhronosGroup/SPIRV-Tools/issues/4892>).
    ///
    /// **Note**: actually obtaining the `debugPrintf` output requires:
    /// * Vulkan Validation Layers (from e.g. the Vulkan SDK)
    ///   * (they contain the `debugPrintf` implementation, a SPIR-V -> SPIR-V translation)
    ///   * **set the `VK_LOADER_LAYERS_ENABLE=VK_LAYER_KHRONOS_validation`
    ///     environment variable** to easily enable them without any code changes
    ///   * alternatively, `"VK_LAYER_KHRONOS_validation"` can be passed during
    ///     instance creation, to enable them programmatically
    /// * Validation Layers' `debugPrintf` support:
    ///   * **set the `VK_LAYER_ENABLES=VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT`
    ///     environment variable** to easily enable the `debugPrintf` support
    ///   * alternatively, `VkValidationFeaturesEXT` during instance creation,
    ///     or the `khronos_validation.enables` field in `vk_layer_settings.txt`,
    ///     can be used to enable `VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT`
    ///     (see also <https://github.com/KhronosGroup/Vulkan-ValidationLayers/blob/main/docs/debug_printf.md>)
    /// * for outputting the `debugPrintf` messages sent back from the GPU:
    ///   * **set the `DEBUG_PRINTF_TO_STDOUT=1` environment variable** if you don't
    ///     plan on customizing the reporting (see below for alternatives)
    /// * for `wgpu`:
    ///   * **required**: `wgpu::Features::SPIRV_SHADER_PASSTHROUGH` (Naga lacks `debugPrintf`)
    ///   * *optional*: building in debug mode (and/or with debug-assertions enabled),
    ///     to enable `wgpu` logging/debug support
    ///     * (the debug assertions requirement may be lifted in future `wgpu` versions)
    ///     * this uses `VK_EXT_debug_utils` internally, and is a better-integrated
    ///       alternative to just setting `DEBUG_PRINTF_TO_STDOUT=1`
    ///     * `RUST_LOG=wgpu_hal::vulkan=info` (or equivalent) will enable said
    ///       output (as `debugPrintf` messages have the "info" level)
    ///     * `RUST_LOG` controls `env_logger`, which isn't itself required,
    ///       but *some* `log`/`tracing` subscriber is needed to get any output
    /// * for Vulkan (e.g. via `ash`):
    ///   * **required**: enabling the `VK_KHR_shader_non_semantic_info` Vulkan *Device* extension
    ///   * *optional*: as described above, enabling the Validation Layers and
    ///     their `debugPrintf` support can be done during instance creation
    ///   * *optional*: integrating [`VK_EXT_debug_utils`](https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VK_EXT_debug_utils.html)
    ///     allows more reporting flexibility than `DEBUG_PRINTF_TO_STDOUT=1`)
    DebugPrintfThenExit {
        /// Whether to also print the entry-point inputs (excluding buffers/resources),
        /// which should uniquely identify the panicking shader invocation.
        print_inputs: bool,

        /// Whether to also print a "backtrace" (i.e. the chain of function calls
        /// that led to the `panic!).
        ///
        /// As there is no way to dynamically compute this information, the string
        /// containing the full backtrace of each `panic!` is statically generated,
        /// meaning this option could significantly increase binary size.
        print_backtrace: bool,
    },

    /// **Warning**: this is _**unsound**_ (i.e. adds Undefined Behavior to *safe* Rust code)
    ///
    /// This option only exists for testing (hence the unfriendly name it has),
    /// and more specifically testing whether conditional panics are responsible
    /// for performance differences when upgrading from older Rust-GPU versions
    /// (which used infinite loops for panics, that `spirv-opt`/drivers could've
    /// sometimes treated as UB, and optimized as if they were impossible to reach).
    ///
    /// Unlike those infinite loops, however, this uses `OpUnreachable`, so it
    /// forces the old worst-case (all `panic!`s become UB and are optimized out).
    #[allow(non_camel_case_types)]
    UNSOUND_DO_NOT_USE_UndefinedBehaviorViaUnreachable,
}

pub struct SpirvBuilder {
    path_to_crate: PathBuf,
    print_metadata: MetadataPrintout,
    release: bool,
    target: String,
    deny_warnings: bool,
    multimodule: bool,
    spirv_metadata: SpirvMetadata,
    capabilities: Vec<Capability>,
    extensions: Vec<String>,
    extra_args: Vec<String>,

    // `rustc_codegen_spirv::linker` codegen args
    pub shader_panic_strategy: ShaderPanicStrategy,

    // spirv-val flags
    pub relax_struct_store: bool,
    pub relax_logical_pointer: bool,
    pub relax_block_layout: bool,
    pub uniform_buffer_standard_layout: bool,
    pub scalar_block_layout: bool,
    pub skip_block_layout: bool,

    // spirv-opt flags
    pub preserve_bindings: bool,
}

impl SpirvBuilder {
    pub fn new(path_to_crate: impl AsRef<Path>, target: impl Into<String>) -> Self {
        Self {
            path_to_crate: path_to_crate.as_ref().to_owned(),
            print_metadata: MetadataPrintout::Full,
            release: true,
            target: target.into(),
            deny_warnings: false,
            multimodule: false,
            spirv_metadata: SpirvMetadata::None,
            capabilities: Vec::new(),
            extensions: Vec::new(),
            extra_args: Vec::new(),

            shader_panic_strategy: ShaderPanicStrategy::SilentExit,

            relax_struct_store: false,
            relax_logical_pointer: false,
            relax_block_layout: false,
            uniform_buffer_standard_layout: false,
            scalar_block_layout: false,
            skip_block_layout: false,

            preserve_bindings: false,
        }
    }

    /// Whether to print build.rs cargo metadata (e.g. cargo:rustc-env=var=val). Defaults to [`MetadataPrintout::Full`].
    #[must_use]
    pub fn print_metadata(mut self, v: MetadataPrintout) -> Self {
        self.print_metadata = v;
        self
    }

    #[must_use]
    pub fn deny_warnings(mut self, v: bool) -> Self {
        self.deny_warnings = v;
        self
    }

    /// Build in release. Defaults to true.
    #[must_use]
    pub fn release(mut self, v: bool) -> Self {
        self.release = v;
        self
    }

    /// Splits the resulting SPIR-V file into one module per entry point. This is useful in cases
    /// where ecosystem tooling has bugs around multiple entry points per module - having all entry
    /// points bundled into a single file is the preferred system.
    #[must_use]
    pub fn multimodule(mut self, v: bool) -> Self {
        self.multimodule = v;
        self
    }

    /// Sets the level of metadata (primarily `OpName` and `OpLine`) included in the SPIR-V binary.
    /// Including metadata significantly increases binary size.
    #[must_use]
    pub fn spirv_metadata(mut self, v: SpirvMetadata) -> Self {
        self.spirv_metadata = v;
        self
    }

    /// Adds a capability to the SPIR-V module. Checking if a capability is enabled in code can be
    /// done via `#[cfg(target_feature = "TheCapability")]`.
    #[must_use]
    pub fn capability(mut self, capability: Capability) -> Self {
        self.capabilities.push(capability);
        self
    }

    /// Adds an extension to the SPIR-V module. Checking if an extension is enabled in code can be
    /// done via `#[cfg(target_feature = "ext:the_extension")]`.
    #[must_use]
    pub fn extension(mut self, extension: impl Into<String>) -> Self {
        self.extensions.push(extension.into());
        self
    }

    /// Change the shader `panic!` handling strategy (see [`ShaderPanicStrategy`]).
    #[must_use]
    pub fn shader_panic_strategy(mut self, shader_panic_strategy: ShaderPanicStrategy) -> Self {
        self.shader_panic_strategy = shader_panic_strategy;
        self
    }

    /// Allow store from one struct type to a different type with compatible layout and members.
    #[must_use]
    pub fn relax_struct_store(mut self, v: bool) -> Self {
        self.relax_struct_store = v;
        self
    }

    /// Allow allocating an object of a pointer type and returning a pointer value from a function
    /// in logical addressing mode
    #[must_use]
    pub fn relax_logical_pointer(mut self, v: bool) -> Self {
        self.relax_logical_pointer = v;
        self
    }

    /// Enable `VK_KHR_relaxed_block_layout` when checking standard uniform, storage buffer, and
    /// push constant layouts. This is the default when targeting Vulkan 1.1 or later.
    #[must_use]
    pub fn relax_block_layout(mut self, v: bool) -> Self {
        self.relax_block_layout = v;
        self
    }

    /// Enable `VK_KHR_uniform_buffer_standard_layout` when checking standard uniform buffer
    /// layouts.
    #[must_use]
    pub fn uniform_buffer_standard_layout(mut self, v: bool) -> Self {
        self.uniform_buffer_standard_layout = v;
        self
    }

    /// Enable `VK_EXT_scalar_block_layout` when checking standard uniform, storage buffer, and
    /// push constant layouts. Scalar layout rules are more permissive than relaxed block layout so
    /// in effect this will override the --relax-block-layout option.
    #[must_use]
    pub fn scalar_block_layout(mut self, v: bool) -> Self {
        self.scalar_block_layout = v;
        self
    }

    /// Skip checking standard uniform/storage buffer layout. Overrides any --relax-block-layout or
    /// --scalar-block-layout option.
    #[must_use]
    pub fn skip_block_layout(mut self, v: bool) -> Self {
        self.skip_block_layout = v;
        self
    }

    /// Preserve unused descriptor bindings. Useful for reflection.
    #[must_use]
    pub fn preserve_bindings(mut self, v: bool) -> Self {
        self.preserve_bindings = v;
        self
    }

    /// Set additional "codegen arg". Note: the `RUSTGPU_CODEGEN_ARGS` environment variable
    /// takes precedence over any set arguments using this function.
    #[must_use]
    pub fn extra_arg(mut self, arg: impl Into<String>) -> Self {
        self.extra_args.push(arg.into());
        self
    }

    /// Builds the module. If `print_metadata` is [`MetadataPrintout::Full`], you usually don't have to inspect the path
    /// in the result, as the environment variable for the path to the module will already be set.
    pub fn build(mut self) -> Result<CompileResult, SpirvBuilderError> {
        self.validate_running_conditions()?;
        let metadata_file = invoke_rustc(&self)?;
        match self.print_metadata {
            MetadataPrintout::Full | MetadataPrintout::DependencyOnly => {
                leaf_deps(&metadata_file, |artifact| {
                    println!("cargo:rerun-if-changed={artifact}");
                })
                // Close enough
                .map_err(SpirvBuilderError::MetadataFileMissing)?;
            }
            MetadataPrintout::None => (),
        }
        let metadata = self.parse_metadata_file(&metadata_file)?;

        Ok(metadata)
    }

    pub(crate) fn validate_running_conditions(&mut self) -> Result<(), SpirvBuilderError> {
        if (self.print_metadata == MetadataPrintout::Full) && self.multimodule {
            return Err(SpirvBuilderError::MultiModuleWithPrintMetadata);
        }
        if !self.path_to_crate.is_dir() {
            return Err(SpirvBuilderError::CratePathDoesntExist(std::mem::take(
                &mut self.path_to_crate,
            )));
        }
        Ok(())
    }

    pub(crate) fn parse_metadata_file(
        &self,
        at: &Path,
    ) -> Result<CompileResult, SpirvBuilderError> {
        let metadata_contents = File::open(at).map_err(SpirvBuilderError::MetadataFileMissing)?;
        let metadata: CompileResult = serde_json::from_reader(BufReader::new(metadata_contents))
            .map_err(SpirvBuilderError::MetadataFileMalformed)?;
        match &metadata.module {
            ModuleResult::SingleModule(spirv_module) => {
                assert!(!self.multimodule);
                let env_var = format!(
                    "{}.spv",
                    at.file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .strip_suffix(".spv.json")
                        .unwrap()
                );
                if self.print_metadata == MetadataPrintout::Full {
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
    panic!("Could not find {filename} in library path");
}

/// Joins strings together while ensuring none of the strings contain the separator.
// NOTE(eddyb) this intentionally consumes the `Vec` to limit accidental misuse.
fn join_checking_for_separators(strings: Vec<impl Borrow<str>>, sep: &str) -> String {
    for s in &strings {
        let s = s.borrow();
        assert!(!s.contains(sep), "{s:?} may not contain separator {sep:?}");
    }
    strings.join(sep)
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

    let mut rustflags = vec![
        format!("-Zcodegen-backend={}", rustc_codegen_spirv.display()),
        // Ensure the codegen backend is emitted in `.d` files to force Cargo
        // to rebuild crates compiled with it when it changes (this used to be
        // the default until https://github.com/rust-lang/rust/pull/93969).
        "-Zbinary-dep-depinfo".to_string(),
        "-Csymbol-mangling-version=v0".to_string(),
        "-Zcrate-attr=feature(register_tool)".to_string(),
        "-Zcrate-attr=register_tool(rust_gpu)".to_string(),
        // HACK(eddyb) this is the same configuration that we test with, and
        // ensures no unwanted surprises from e.g. `core` debug assertions.
        "-Coverflow-checks=off".to_string(),
        "-Cdebug-assertions=off".to_string(),
        // HACK(eddyb) we need this for `core::fmt::rt::Argument::new_*` calls
        // to *never* be inlined, so we can pattern-match the calls themselves.
        "-Zinline-mir=off".to_string(),
    ];

    // Wrapper for `env::var` that appropriately informs Cargo of the dependency.
    let tracked_env_var_get = |name| {
        if let MetadataPrintout::Full | MetadataPrintout::DependencyOnly = builder.print_metadata {
            println!("cargo:rerun-if-env-changed={name}");
        }
        env::var(name)
    };

    let mut llvm_args = vec![];
    if builder.multimodule {
        llvm_args.push("--module-output=multiple".to_string());
    }
    match builder.spirv_metadata {
        SpirvMetadata::None => (),
        SpirvMetadata::NameVariables => {
            llvm_args.push("--spirv-metadata=name-variables".to_string());
        }
        SpirvMetadata::Full => llvm_args.push("--spirv-metadata=full".to_string()),
    }
    if builder.relax_struct_store {
        llvm_args.push("--relax-struct-store".to_string());
    }
    if builder.relax_logical_pointer {
        llvm_args.push("--relax-logical-pointer".to_string());
    }
    if builder.relax_block_layout {
        llvm_args.push("--relax-block-layout".to_string());
    }
    if builder.uniform_buffer_standard_layout {
        llvm_args.push("--uniform-buffer-standard-layout".to_string());
    }
    if builder.scalar_block_layout {
        llvm_args.push("--scalar-block-layout".to_string());
    }
    if builder.skip_block_layout {
        llvm_args.push("--skip-block-layout".to_string());
    }
    if builder.preserve_bindings {
        llvm_args.push("--preserve-bindings".to_string());
    }
    let mut target_features = vec![];
    let abort_strategy = match builder.shader_panic_strategy {
        ShaderPanicStrategy::SilentExit => None,
        ShaderPanicStrategy::DebugPrintfThenExit {
            print_inputs,
            print_backtrace,
        } => {
            target_features.push("+ext:SPV_KHR_non_semantic_info".into());
            Some(format!(
                "debug-printf{}{}",
                if print_inputs { "+inputs" } else { "" },
                if print_backtrace { "+backtrace" } else { "" }
            ))
        }
        ShaderPanicStrategy::UNSOUND_DO_NOT_USE_UndefinedBehaviorViaUnreachable => {
            Some("unreachable".into())
        }
    };
    llvm_args.extend(abort_strategy.map(|strategy| format!("--abort-strategy={strategy}")));

    if let Ok(extra_codegen_args) = tracked_env_var_get("RUSTGPU_CODEGEN_ARGS") {
        llvm_args.extend(extra_codegen_args.split_whitespace().map(|s| s.to_string()));
    } else {
        llvm_args.extend(builder.extra_args.iter().cloned());
    }

    let llvm_args = join_checking_for_separators(llvm_args, " ");
    if !llvm_args.is_empty() {
        rustflags.push(["-Cllvm-args=", &llvm_args].concat());
    }

    target_features.extend(builder.capabilities.iter().map(|cap| format!("+{cap:?}")));
    target_features.extend(builder.extensions.iter().map(|ext| format!("+ext:{ext}")));
    let target_features = join_checking_for_separators(target_features, ",");
    if !target_features.is_empty() {
        rustflags.push(["-Ctarget-feature=", &target_features].concat());
    }

    if builder.deny_warnings {
        rustflags.push("-Dwarnings".to_string());
    }

    if let Ok(extra_rustflags) = tracked_env_var_get("RUSTGPU_RUSTFLAGS") {
        rustflags.extend(extra_rustflags.split_whitespace().map(|s| s.to_string()));
    }

    // If we're nested in `cargo` invocation, use a different `--target-dir`,
    // to avoid waiting on the same lock (which effectively dead-locks us).
    let outer_target_dir = match (env::var("PROFILE"), env::var_os("OUT_DIR")) {
        (Ok(outer_profile), Some(dir)) => {
            // Strip `$outer_profile/build/*/out`.
            [&outer_profile, "build", "*", "out"].iter().rev().try_fold(
                PathBuf::from(dir),
                |mut dir, &filter| {
                    if (filter == "*" || dir.ends_with(filter)) && dir.pop() {
                        Some(dir)
                    } else {
                        None
                    }
                },
            )
        }
        _ => None,
    };
    // FIXME(eddyb) use `crate metadata` to always be able to get the "outer"
    // (or "default") `--target-dir`, to append `/spirv-builder` to it.
    let target_dir = outer_target_dir.map(|outer| outer.join("spirv-builder"));

    let profile = if builder.release { "release" } else { "dev" };

    let mut cargo = Command::new("cargo");
    cargo.args([
        "build",
        "--lib",
        "--message-format=json-render-diagnostics",
        "-Zbuild-std=core",
        "-Zbuild-std-features=compiler-builtins-mem",
        "--profile",
        profile,
        "--target",
        &*builder.target,
    ]);

    // NOTE(eddyb) see above how this is computed and why it might be missing.
    if let Some(target_dir) = target_dir {
        cargo.arg("--target-dir").arg(target_dir);
    }

    // Clear Cargo environment variables that we don't want to leak into the
    // inner invocation of Cargo (because e.g. build scripts might read them),
    // before we set any of our own below.
    for (key, _) in env::vars_os() {
        let remove = key.to_str().map_or(false, |s| {
            s.starts_with("CARGO_FEATURES_") || s.starts_with("CARGO_CFG_")
        });
        if remove {
            cargo.env_remove(key);
        }
    }

    // NOTE(eddyb) Cargo caches some information it got from `rustc` in
    // `.rustc_info.json`, and assumes it only depends on the `rustc` binary,
    // but in our case, `rustc_codegen_spirv` changes are also relevant,
    // so we turn off that caching with an env var, just to avoid any issues.
    cargo.env("CARGO_CACHE_RUSTC_INFO", "0");

    // NOTE(eddyb) this used to be just `RUSTFLAGS` but at some point Cargo
    // added a separate environment variable using `\x1f` instead of spaces,
    // which allows us to have spaces within individual `rustc` flags.
    cargo.env(
        "CARGO_ENCODED_RUSTFLAGS",
        join_checking_for_separators(rustflags, "\x1f"),
    );

    let profile_in_env_var = profile.replace('-', "_").to_ascii_uppercase();

    // NOTE(eddyb) there's no parallelism to take advantage of multiple CGUs,
    // and inter-CGU duplication can be wasteful, so this forces 1 CGU for now.
    let num_cgus = 1;
    cargo.env(
        format!("CARGO_PROFILE_{profile_in_env_var}_CODEGEN_UNITS"),
        num_cgus.to_string(),
    );

    let build = cargo
        .stderr(Stdio::inherit())
        .current_dir(&builder.path_to_crate)
        .output()
        .expect("failed to execute cargo build");

    // `get_last_artifact` has the side-effect of printing invalid lines, so
    // we do that even in case of an error, to let through any useful messages
    // that ended up on stdout instead of stderr.
    let stdout = String::from_utf8(build.stdout).unwrap();
    if build.status.success() {
        get_sole_artifact(&stdout).ok_or_else(|| {
            eprintln!("--- build output ---\n{stdout}");
            panic!(
                "`{ARTIFACT_SUFFIX}` artifact not found in (supposedly successful) build output (see above)"
            );
        })
    } else {
        Err(SpirvBuilderError::BuildFailed)
    }
}

#[derive(Deserialize)]
struct RustcOutput {
    reason: String,
    filenames: Option<Vec<String>>,
}

const ARTIFACT_SUFFIX: &str = ".spv.json";

fn get_sole_artifact(out: &str) -> Option<PathBuf> {
    let last = out
        .lines()
        .filter_map(|line| {
            if let Ok(line) = serde_json::from_str::<RustcOutput>(line) {
                Some(line)
            } else {
                // Pass through invalid lines
                println!("{line}");
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
        .filter(|v| v.ends_with(ARTIFACT_SUFFIX));
    let filename = filenames.next()?;
    assert_eq!(
        filenames.next(),
        None,
        "build had multiple `{ARTIFACT_SUFFIX}` artifacts"
    );
    Some(filename.into())
}

/// Internally iterate through the leaf dependencies of the artifact at `artifact`
fn leaf_deps(artifact: &Path, mut handle: impl FnMut(&RawStr)) -> std::io::Result<()> {
    let deps_file = artifact.with_extension("d");
    let mut deps_map = HashMap::new();
    depfile::read_deps_file(&deps_file, |item, deps| {
        deps_map.insert(item, deps);
        Ok(())
    })?;
    fn recurse(
        map: &HashMap<RawString, Vec<RawString>>,
        artifact: &RawStr,
        handle: &mut impl FnMut(&RawStr),
    ) {
        match map.get(artifact) {
            Some(entries) => {
                for entry in entries {
                    recurse(map, entry, handle);
                }
            }
            None => handle(artifact),
        }
    }
    recurse(&deps_map, artifact.to_str().unwrap().into(), &mut handle);
    Ok(())
}
