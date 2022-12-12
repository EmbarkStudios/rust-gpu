use std::{
    env,
    io::{Error, ErrorKind, Result},
    path::{Path, PathBuf},
};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(
    name = "cargo compiletest",
    no_version,
    // HACK(eddyb) avoid "USAGE:" saying "compiletests".
    usage = "cargo compiletest [FLAGS] [FILTER]..."
)]
struct Opt {
    /// Automatically update stderr/stdout files.
    #[structopt(long)]
    bless: bool,

    /// The environment to compile to the SPIR-V tests.
    #[structopt(long, default_value = "spv1.3")]
    target_env: String,

    /// Only run tests that match these filters.
    #[structopt(name = "FILTER")]
    filters: Vec<String>,
}

impl Opt {
    pub fn environments(&self) -> impl Iterator<Item = &str> {
        self.target_env.split(',')
    }
}

const TARGET_PREFIX: &str = "spirv-unknown-";

#[derive(Copy, Clone)]
enum DepKind {
    SpirvLib,
    ProcMacro,
}

impl DepKind {
    fn prefix_and_extension(self) -> (&'static str, &'static str) {
        match self {
            Self::SpirvLib => ("lib", "rlib"),
            Self::ProcMacro => (env::consts::DLL_PREFIX, env::consts::DLL_EXTENSION),
        }
    }

    fn target_dir_suffix(self, target: &str) -> String {
        match self {
            Self::SpirvLib => format!("{}/debug/deps", target),
            Self::ProcMacro => "debug/deps".into(),
        }
    }
}

fn main() {
    let opt = Opt::from_args();

    let tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = tests_dir.parent().unwrap();
    let original_target_dir = workspace_root.join("target");
    let deps_target_dir = original_target_dir.join("compiletest-deps");
    let compiletest_build_dir = original_target_dir.join("compiletest-results");

    // Pull in rustc_codegen_spirv as a dynamic library in the same way
    // spirv-builder does.
    let codegen_backend_path = find_rustc_codegen_spirv();

    let runner = Runner {
        opt,
        tests_dir,
        compiletest_build_dir,
        deps_target_dir,
        codegen_backend_path,
    };

    runner.run_mode("ui");
}

struct Runner {
    opt: Opt,
    tests_dir: PathBuf,
    compiletest_build_dir: PathBuf,
    deps_target_dir: PathBuf,
    codegen_backend_path: PathBuf,
}

impl Runner {
    /// Runs the given `mode` on the directory that matches that name, using the
    /// backend provided by `codegen_backend_path`.
    #[allow(clippy::string_add)]
    fn run_mode(&self, mode: &'static str) {
        /// RUSTFLAGS passed to all test files.
        fn test_rustc_flags(
            codegen_backend_path: &Path,
            deps: &TestDeps,
            indirect_deps_dirs: &[&Path],
        ) -> String {
            [
                &*rust_flags(codegen_backend_path),
                &*indirect_deps_dirs
                    .iter()
                    .map(|dir| format!("-L dependency={}", dir.display()))
                    .fold(String::new(), |a, b| b + " " + &a),
                "--edition 2018",
                &*format!("--extern noprelude:core={}", deps.core.display()),
                &*format!(
                    "--extern noprelude:compiler_builtins={}",
                    deps.compiler_builtins.display()
                ),
                &*format!(
                    "--extern spirv_std_macros={}",
                    deps.spirv_std_macros.display()
                ),
                &*format!("--extern spirv_std={}", deps.spirv_std.display()),
                &*format!("--extern glam={}", deps.glam.display()),
                "--crate-type dylib",
                "-Zunstable-options",
                "-Zcrate-attr=no_std",
                "-Zcrate-attr=feature(asm_const,asm_experimental_arch)",
            ]
            .join(" ")
        }

        for (env, spirt) in self
            .opt
            .environments()
            .flat_map(|env| [(env, false), (env, true)])
        {
            // HACK(eddyb) in order to allow *some* tests to have separate output
            // with the SPIR-T support enabled (via `--spirt`), while keeping
            // *most* of the tests unchanged, we take advantage of "stage IDs",
            // which offer `// only-S` and `// ignore-S` for any stage ID `S`.
            let stage_id = if spirt { "spirt" } else { "not_spirt" };

            let target = format!("{}{}", TARGET_PREFIX, env);
            let libs = build_deps(&self.deps_target_dir, &self.codegen_backend_path, &target);
            let mut flags = test_rustc_flags(
                &self.codegen_backend_path,
                &libs,
                &[
                    &self
                        .deps_target_dir
                        .join(DepKind::SpirvLib.target_dir_suffix(&target)),
                    &self
                        .deps_target_dir
                        .join(DepKind::ProcMacro.target_dir_suffix(&target)),
                ],
            );
            if spirt {
                flags += " -Cllvm-args=--spirt";
            }

            let config = compiletest::Config {
                stage_id: stage_id.to_string(),
                target_rustcflags: Some(flags),
                mode: mode.parse().expect("Invalid mode"),
                target,
                src_base: self.tests_dir.join(mode),
                build_base: self.compiletest_build_dir.clone(),
                bless: self.opt.bless,
                filters: self.opt.filters.clone(),
                ..compiletest::Config::default()
            };
            // FIXME(eddyb) do we need this? shouldn't `compiletest` be independent?
            config.clean_rmeta();

            compiletest::run_tests(&config);
        }
    }
}

/// Runs the processes needed to build `spirv-std` & other deps.
fn build_deps(deps_target_dir: &Path, codegen_backend_path: &Path, target: &str) -> TestDeps {
    // HACK(eddyb) this is only needed until we enable `resolver = "2"`, as the
    // old ("1") resolver has a bug where it picks up extra features based on the
    // current directory (and so we always set the working dir as a workaround).
    let old_cargo_resolver_workaround_cwd = deps_target_dir.parent().unwrap();

    // Build compiletests-deps-helper
    std::process::Command::new("cargo")
        .args([
            "build",
            "-p",
            "compiletests-deps-helper",
            "-Zbuild-std=core",
            "-Zbuild-std-features=compiler-builtins-mem",
            &*format!("--target={}", target),
        ])
        .arg("--target-dir")
        .arg(deps_target_dir)
        .env("RUSTFLAGS", rust_flags(codegen_backend_path))
        .current_dir(old_cargo_resolver_workaround_cwd)
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()
        .and_then(map_status_to_result)
        .unwrap();

    let compiler_builtins = find_lib(
        deps_target_dir,
        "compiler_builtins",
        DepKind::SpirvLib,
        target,
    )
    .unwrap();
    let core = find_lib(deps_target_dir, "core", DepKind::SpirvLib, target).unwrap();
    let spirv_std = find_lib(deps_target_dir, "spirv_std", DepKind::SpirvLib, target).unwrap();
    let glam = find_lib(deps_target_dir, "glam", DepKind::SpirvLib, target).unwrap();
    let spirv_std_macros = find_lib(
        deps_target_dir,
        "spirv_std_macros",
        DepKind::ProcMacro,
        target,
    )
    .unwrap();

    if [
        &compiler_builtins,
        &core,
        &spirv_std,
        &glam,
        &spirv_std_macros,
    ]
    .iter()
    .any(|o| o.is_none())
    {
        clean_deps(deps_target_dir);
        build_deps(deps_target_dir, codegen_backend_path, target)
    } else {
        TestDeps {
            core: core.unwrap(),
            glam: glam.unwrap(),
            compiler_builtins: compiler_builtins.unwrap(),
            spirv_std: spirv_std.unwrap(),
            spirv_std_macros: spirv_std_macros.unwrap(),
        }
    }
}

fn clean_deps(deps_target_dir: &Path) {
    std::process::Command::new("cargo")
        .arg("clean")
        .arg("--target-dir")
        .arg(deps_target_dir)
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()
        .and_then(map_status_to_result)
        .unwrap();
}

/// Attempt find the rlib that matches `base`, if multiple rlibs are found
/// then a clean build is required and `None` is returned.
fn find_lib(
    deps_target_dir: &Path,
    base: impl AsRef<Path>,
    dep_kind: DepKind,
    target: &str,
) -> Result<Option<PathBuf>> {
    let base = base.as_ref();
    let (expected_prefix, expected_extension) = dep_kind.prefix_and_extension();
    let expected_name = format!("{}{}", expected_prefix, base.display());

    let dir = deps_target_dir.join(dep_kind.target_dir_suffix(target));

    let paths = std::fs::read_dir(dir)?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            let name = {
                let name = path.file_stem();
                if name.is_none() {
                    return false;
                }
                name.unwrap()
            };

            let name_matches = name.to_str().unwrap().starts_with(&expected_name)
                && name.len() == expected_name.len() + 17   // we expect our name, '-', and then 16 hexadecimal digits
                && ends_with_dash_hash(name.to_str().unwrap());
            let extension_matches = path
                .extension()
                .map_or(false, |ext| ext == expected_extension);

            name_matches && extension_matches
        })
        .collect::<Vec<_>>();

    Ok(if paths.len() > 1 {
        None
    } else {
        paths.into_iter().next()
    })
}

/// Returns whether this string ends with a dash ('-'), followed by 16 lowercase hexadecimal characters
fn ends_with_dash_hash(s: &str) -> bool {
    let n = s.len();
    if n < 17 {
        return false;
    }
    let mut bytes = s.bytes().skip(n - 17);
    if bytes.next() != Some(b'-') {
        return false;
    }

    bytes.all(|b| b.is_ascii_hexdigit())
}

/// Paths to all of the library artifacts of dependencies needed to compile tests.
struct TestDeps {
    core: PathBuf,
    compiler_builtins: PathBuf,
    spirv_std: PathBuf,
    spirv_std_macros: PathBuf,
    glam: PathBuf,
}

/// The RUSTFLAGS passed to all SPIR-V builds.
// FIXME(eddyb) expose most of these from `spirv-builder`.
fn rust_flags(codegen_backend_path: &Path) -> String {
    let target_features = [
        "Int8",
        "Int16",
        "Int64",
        "Float64",
        // Only needed for `ui/arch/read_clock_khr.rs`.
        "ShaderClockKHR",
        "ext:SPV_KHR_shader_clock",
    ];

    [
        &*format!("-Zcodegen-backend={}", codegen_backend_path.display()),
        // Ensure the codegen backend is emitted in `.d` files to force Cargo
        // to rebuild crates compiled with it when it changes (this used to be
        // the default until https://github.com/rust-lang/rust/pull/93969).
        "-Zbinary-dep-depinfo",
        "-Coverflow-checks=off",
        "-Cdebug-assertions=off",
        "-Cdebuginfo=2",
        "-Cembed-bitcode=no",
        &format!("-Ctarget-feature=+{}", target_features.join(",+")),
        "-Csymbol-mangling-version=v0",
        "-Zcrate-attr=feature(register_tool)",
        "-Zcrate-attr=register_tool(rust_gpu)",
    ]
    .join(" ")
}

/// Convience function to map process failure to results in Rust.
fn map_status_to_result(status: std::process::ExitStatus) -> Result<()> {
    match status.success() {
        true => Ok(()),
        false => Err(Error::new(
            ErrorKind::Other,
            format!(
                "process terminated with non-zero code: {}",
                status.code().unwrap_or(0)
            ),
        )),
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
