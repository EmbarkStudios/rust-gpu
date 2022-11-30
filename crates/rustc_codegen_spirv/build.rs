//! This custom build script merely checks whether we're compiling with the appropriate Rust toolchain

#![allow(clippy::string_add)]

use std::error::Error;
use std::process::{Command, ExitCode};

/// Current `rust-toolchain` file
/// Unfortunately, directly including the actual workspace `rust-toolchain` doesn't work together with
/// `cargo publish`. We need to figure out a way to do this properly, but let's hardcode it for now :/
//const REQUIRED_RUST_TOOLCHAIN: &str = include_str!("../../rust-toolchain");
const REQUIRED_RUST_TOOLCHAIN: &str = r#"[toolchain]
channel = "nightly-2022-10-29"
components = ["rust-src", "rustc-dev", "llvm-tools-preview"]
# commit_hash = 9565dfeb4e6225177bbe78f18cd48a7982f34401"#;

fn get_rustc_commit_hash() -> Result<String, Box<dyn Error>> {
    let rustc = std::env::var("RUSTC").unwrap_or_else(|_| String::from("rustc"));
    String::from_utf8(Command::new(rustc).arg("-vV").output()?.stdout)?
        .lines()
        .find_map(|l| l.strip_prefix("commit-hash: "))
        .map(|s| s.to_string())
        .ok_or_else(|| Box::<dyn Error>::from("`commit-hash` not found in `rustc -vV` output"))
}

fn get_required_commit_hash() -> Result<String, Box<dyn Error>> {
    REQUIRED_RUST_TOOLCHAIN
        .lines()
        .find_map(|l| l.strip_prefix("# commit_hash = "))
        .map(|s| s.to_string())
        .ok_or_else(|| Box::<dyn Error>::from("`commit_hash` not found in `rust-toolchain`"))
}

fn check_toolchain_version() -> Result<(), Box<dyn Error>> {
    // make sure we rebuild if RUSTGPU_SKIP_TOOLCHAIN_CHECK env var changes
    println!("cargo:rerun-if-env-changed=RUSTGPU_SKIP_TOOLCHAIN_CHECK");

    // if we're building from local source, check if REQUIRED_RUST_TOOLCHAIN matches ../../rust-toolchain
    if std::env::current_dir()?.ends_with("crates/rustc_codegen_spirv") {
        let current_toolchain = std::fs::read_to_string("../../rust-toolchain")?;
        if !current_toolchain.contains(REQUIRED_RUST_TOOLCHAIN) {
            return Err(Box::<dyn Error>::from(format!(
                "error: building from local source while `REQUIRED_RUST_TOOLCHAIN` (defined in `{}`) doesn't match `{}`",
                file!(),
                std::path::Path::new("../../rust-toolchain").canonicalize()?.display()
            )));
        }
    }

    if !cfg!(feature = "skip-toolchain-check")
        && std::env::var("RUSTGPU_SKIP_TOOLCHAIN_CHECK").is_err()
    {
        // check if our current rustc's commit hash matches with what we expect it to be
        let current_hash = get_rustc_commit_hash()?;
        let required_hash = get_required_commit_hash()?;
        if current_hash != required_hash {
            let stripped_toolchain = REQUIRED_RUST_TOOLCHAIN
                .lines()
                .filter(|l| !l.trim().is_empty() && !l.starts_with("# "))
                .map(|l| l.to_string())
                .reduce(|a, b| a + "\n" + &b)
                .unwrap_or_default();

            return Err(Box::<dyn Error>::from(format!(
                r#"error: wrong toolchain detected (found commit hash `{current_hash}`, expected `{required_hash}`).
Make sure your `rust_toolchain` file contains the following:
-------------
{stripped_toolchain}
-------------"#
            )));
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    match check_toolchain_version() {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprint!("{}", e);
            ExitCode::FAILURE
        }
    }
}
