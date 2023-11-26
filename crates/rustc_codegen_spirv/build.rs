//! This custom build script merely checks whether we're compiling with the appropriate Rust toolchain

#![allow(clippy::string_add)]

use std::error::Error;
use std::process::{Command, ExitCode};

const REQUIRED_RUST_TOOLCHAIN: &str = include_str!("rust-toolchain.toml");

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
        .ok_or_else(|| Box::<dyn Error>::from("`commit_hash` not found in `rust-toolchain.toml`"))
}

fn check_toolchain_version() -> Result<(), Box<dyn Error>> {
    // make sure we rebuild if RUSTGPU_SKIP_TOOLCHAIN_CHECK env var changes
    println!("cargo:rerun-if-env-changed=RUSTGPU_SKIP_TOOLCHAIN_CHECK");

    if std::env::var("RUSTGPU_SKIP_TOOLCHAIN_CHECK").is_err() {
        // check if our current rustc's commit hash matches with what we expect it to be
        let current_hash = get_rustc_commit_hash()?;
        let required_hash = get_required_commit_hash()?;
        if current_hash != required_hash {
            return Err(Box::<dyn Error>::from(format!(
                "error: wrong toolchain (found commit hash `{current_hash}`, expected `{required_hash}`).\n\
                 note: `spirv-builder` should've prevented this mismatch\n\
                 help: if you're enabling `spirv-builder` internal features, remove them and retry"
            )));
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    match check_toolchain_version() {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprint!("{e}");
            ExitCode::FAILURE
        }
    }
}
