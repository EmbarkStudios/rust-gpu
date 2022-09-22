//! This custom build script merely checks whether we're compiling with the appropriate Rust toolchain

#![allow(clippy::string_add)]

use std::error::Error;
use std::process::{Command, ExitCode};

/// Current `rust-toolchain` file
/// Unfortunately, directly including the actual workspace `rust-toolchain` doesn't work together with
/// `cargo publish`. We need to figure out a way to do this properly, but let's hardcode it for now :/
//const REQUIRED_RUST_TOOLCHAIN: &str = include_str!("../../rust-toolchain");
const REQUIRED_RUST_TOOLCHAIN: &str = r#"[toolchain]
channel = "nightly-2022-08-29"
components = ["rust-src", "rustc-dev", "llvm-tools-preview"]
# commit_hash = ce36e88256f09078519f8bc6b21e4dc88f88f523"#;

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
    if !cfg!(feature = "skip-toolchain-check") {
        // gets the commit hash from current rustc

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
                r#"
error: wrong toolchain detected (found commit hash `{current_hash}`, expected `{required_hash}`).
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
    if let Err(e) = check_toolchain_version() {
        eprintln!(
            "{}",
            std::env::vars()
                .map(|v| v.0 + "=" + &v.1)
                .reduce(|a, b| a + "\n" + &b)
                .unwrap_or_default()
        );
        eprint!("{}", e);
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}
