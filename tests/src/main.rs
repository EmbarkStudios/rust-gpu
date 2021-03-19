use std::{
    env,
    io::{Error, ErrorKind, Result},
    path::{Path, PathBuf},
};

const TARGET: &str = "spirv-unknown-unknown";
const TARGET_DIR: &str = "target/compiletest";
const SPIRV_STD_TARGET: &str = "target/compiletest/spirv-std";
const SPIRV_STD_HOST_DEPS: &str = "target/compiletest/spirv-std/debug/deps";
const SPIRV_STD_TARGET_DEPS: &str = "target/compiletest/spirv-std/spirv-unknown-unknown/debug/deps";

fn main() {
    let manifest_dir = PathBuf::from("./");
    std::env::set_var("CARGO_MANIFEST_DIR", &manifest_dir);
    // Pull in rustc_codegen_spirv as a dynamic library in the same way
    // spirv-builder does.
    let codegen_backend_path = find_rustc_codegen_spirv();
    let libs = build_spirv_std(&manifest_dir, &codegen_backend_path);

    run_mode("ui", &codegen_backend_path, &libs);
}

/// Runs the given `mode` on the directory that matches that name, using the
/// backend provided by `codegen_backend_path`.
fn run_mode(mode: &'static str, codegen_backend_path: &Path, libs: &TestDeps) {
    let mut config = compiletest::Config::default();

    /// RUSTFLAGS passed to all test files.
    fn test_rustc_flags(codegen_backend_path: &Path, deps: &TestDeps, libs: &[&Path]) -> String {
        [
            &*rust_flags(codegen_backend_path),
            &*libs
                .iter()
                .map(|p| format!("-L {}", p.display()))
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
            &PathBuf::from(format!("dependency={}", SPIRV_STD_TARGET_DEPS)),
            &PathBuf::from(format!("dependency={}", SPIRV_STD_HOST_DEPS)),
        ],
    );

    config.target_rustcflags = Some(flags);
    config.mode = mode.parse().expect("Invalid mode");
    config.target = String::from(TARGET);
    config.src_base = PathBuf::from(format!("./tests/{}", mode));
    config.build_base = PathBuf::from(format!("./{}-results", TARGET_DIR));
    config.bless = std::env::args().any(|a| a == "--bless");
    config.clean_rmeta();

    compiletest::run_tests(&config);
}

/// Runs the processes needed to build `spirv-std`.
fn build_spirv_std(manifest_dir: &Path, codegen_backend_path: &Path) -> TestDeps {
    let target_dir = format!("--target-dir={}", SPIRV_STD_TARGET);

    // Build compiletests-deps-helper
    std::process::Command::new("cargo")
        .args(&[
            "build",
            "-p",
            "compiletests-deps-helper",
            "-Zbuild-std=core",
            &*format!("--target={}", TARGET),
            &*target_dir,
        ])
        .env("RUSTFLAGS", rust_flags(&codegen_backend_path))
        .env("CARGO_MANIFEST_DIR", manifest_dir)
        .current_dir(manifest_dir)
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()
        .and_then(map_status_to_result)
        .unwrap();

    let compiler_builtins = find_lib(SPIRV_STD_TARGET_DEPS, "libcompiler_builtins", false).unwrap();
    let core = find_lib(SPIRV_STD_TARGET_DEPS, "libcore", false).unwrap();
    let spirv_std = find_lib(SPIRV_STD_TARGET_DEPS, "libspirv_std", false).unwrap();
    let glam = find_lib(SPIRV_STD_TARGET_DEPS, "libglam", false).unwrap();
    let spirv_std_macros = find_lib(SPIRV_STD_HOST_DEPS, "spirv_std_macros", true).unwrap();

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
        clean_project(manifest_dir);
        build_spirv_std(manifest_dir, codegen_backend_path)
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

fn clean_project(manifest_dir: &Path) {
    std::process::Command::new("cargo")
        .args(&["clean", &*format!("--target-dir={}", TARGET_DIR)])
        .current_dir(manifest_dir)
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()
        .and_then(map_status_to_result)
        .unwrap();
}

/// Attempt find the rlib that matches `base`, if multiple rlibs are found
/// then a clean build is required and `None` is returned.
fn find_lib(
    dir: impl AsRef<Path>,
    base: impl AsRef<Path>,
    dynamic: bool,
) -> Result<Option<PathBuf>> {
    let base = base.as_ref();
    let expected_name = if dynamic {
        format!("{}{}", env::consts::DLL_PREFIX, base.display())
    } else {
        base.display().to_string()
    };

    let paths = std::fs::read_dir(dir.as_ref())?
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
            let extension_matches = path.extension().map_or(false, |ext| {
                if dynamic {
                    ext == env::consts::DLL_EXTENSION
                } else {
                    ext == "rlib"
                }
            });

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
