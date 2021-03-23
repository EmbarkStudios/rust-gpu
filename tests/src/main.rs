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
    /// Automatically update stderr/stdout files
    #[structopt(long)]
    bless: bool,

    /// Only run tests that match these filters
    #[structopt(name = "FILTER")]
    filters: Vec<String>,
}

const TARGET: &str = "spirv-unknown-unknown";

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

    fn target_dir_suffix(self) -> &'static str {
        match self {
            Self::SpirvLib => "spirv-unknown-unknown/debug/deps",
            Self::ProcMacro => "debug/deps",
        }
    }
}

fn main() {
    let opt = Opt::from_args();

    let tests_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = tests_dir.parent().unwrap();
    let original_target_dir = workspace_root.join("target");
    let deps_target_dir = original_target_dir.join("compiletest-deps");
    let compiletest_build_dir = original_target_dir.join("compiletest-results");

    // Pull in rustc_codegen_spirv as a dynamic library in the same way
    // spirv-builder does.
    let codegen_backend_path = find_rustc_codegen_spirv();
    let libs = build_deps(&deps_target_dir, &codegen_backend_path);

    run_mode(
        "ui",
        opt,
        tests_dir,
        compiletest_build_dir,
        &deps_target_dir,
        &codegen_backend_path,
        &libs,
    );
}

// FIXME(eddyb) a bunch of these functions could be nicer if they were methods.

/// Runs the given `mode` on the directory that matches that name, using the
/// backend provided by `codegen_backend_path`.
fn run_mode(
    mode: &'static str,
    opt: Opt,
    tests_dir: &Path,
    compiletest_build_dir: PathBuf,
    deps_target_dir: &Path,
    codegen_backend_path: &Path,
    libs: &TestDeps,
) {
    let mut config = compiletest::Config::default();

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
            "-Zcrate-attr=feature(register_attr,asm)",
            "-Zcrate-attr=register_attr(spirv)",
        ]
        .join(" ")
    }

    let flags = test_rustc_flags(
        codegen_backend_path,
        libs,
        &[
            &deps_target_dir.join(DepKind::SpirvLib.target_dir_suffix()),
            &deps_target_dir.join(DepKind::ProcMacro.target_dir_suffix()),
        ],
    );

    config.target_rustcflags = Some(flags);
    config.mode = mode.parse().expect("Invalid mode");
    config.target = String::from(TARGET);
    config.src_base = tests_dir.join(mode);
    config.build_base = compiletest_build_dir;
    config.bless = opt.bless;
    config.filters = opt.filters;
    config.clean_rmeta();

    compiletest::run_tests(&config);
}

/// Runs the processes needed to build `spirv-std` & other deps.
fn build_deps(deps_target_dir: &Path, codegen_backend_path: &Path) -> TestDeps {
    // HACK(eddyb) this is only needed until we enable `resolver = "2"`, as the
    // old ("1") resolver has a bug where it picks up extra features based on the
    // current directory (and so we always set the working dir as a workaround).
    let old_cargo_resolver_workaround_cwd = deps_target_dir.parent().unwrap();

    // Build compiletests-deps-helper
    std::process::Command::new("cargo")
        .args(&[
            "build",
            "-p",
            "compiletests-deps-helper",
            "-Zbuild-std=core",
            &*format!("--target={}", TARGET),
        ])
        .arg("--target-dir")
        .arg(deps_target_dir)
        .env("RUSTFLAGS", rust_flags(&codegen_backend_path))
        .current_dir(old_cargo_resolver_workaround_cwd)
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()
        .and_then(map_status_to_result)
        .unwrap();

    let compiler_builtins =
        find_lib(deps_target_dir, "compiler_builtins", DepKind::SpirvLib).unwrap();
    let core = find_lib(deps_target_dir, "core", DepKind::SpirvLib).unwrap();
    let spirv_std = find_lib(deps_target_dir, "spirv_std", DepKind::SpirvLib).unwrap();
    let glam = find_lib(deps_target_dir, "glam", DepKind::SpirvLib).unwrap();
    let spirv_std_macros =
        find_lib(deps_target_dir, "spirv_std_macros", DepKind::ProcMacro).unwrap();

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
        build_deps(deps_target_dir, codegen_backend_path)
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
) -> Result<Option<PathBuf>> {
    let base = base.as_ref();
    let (expected_prefix, expected_extension) = dep_kind.prefix_and_extension();
    let expected_name = format!("{}{}", expected_prefix, base.display());

    let dir = deps_target_dir.join(dep_kind.target_dir_suffix());

    let paths = std::fs::read_dir(dir)?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            let name = {
                let name = path.file_name();
                if name.is_none() {
                    return false;
                }
                name.unwrap()
            };

            let name_matches = name.to_str().unwrap().starts_with(&expected_name);
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

/// Paths to all of the library artifacts of dependencies needed to compile tests.
struct TestDeps {
    core: PathBuf,
    compiler_builtins: PathBuf,
    spirv_std: PathBuf,
    spirv_std_macros: PathBuf,
    glam: PathBuf,
}

/// The RUSTFLAGS passed to all SPIR-V builds.
fn rust_flags(codegen_backend_path: &Path) -> String {
    [
        &*format!("-Zcodegen-backend={}", codegen_backend_path.display()),
        "-Coverflow-checks=off",
        "-Cdebug-assertions=off",
        "-Cdebuginfo=2",
        "-Cembed-bitcode=no",
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
