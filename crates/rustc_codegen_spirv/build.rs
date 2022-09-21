//! This custom build script merely checks whether we're compiling with the appropriate Rust toolchain

#[allow(unused_imports)]
use colored::*;
use std::process::Command;

/// The required toolchain version for this package. Update accordingly.
#[cfg(not(feature = "skip-toolchain-check"))]
const REQUIRED_TOOLCHAIN: &str = "nightly-2022-08-29";

/// The required toolchain commit hash for this package. Update accordingly.
#[cfg(not(feature = "skip-toolchain-check"))]
const REQUIRED_COMMIT_HASH: &str = "ce36e88256f09078519f8bc6b21e4dc88f88f523";

#[cfg(not(feature = "skip-toolchain-check"))]
fn get_rustc_commit_hash() -> Option<String> {
    let rustc = std::env::var("RUSTC").unwrap_or(String::from("rustc"));
    Command::new(rustc)
        .arg("-vV")
        .output()
        .ok()
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .map(|s| {
            for l in s.lines() {
                match l.strip_prefix("commit-hash: ") {
                    Some(hash) => {
                        return Some(hash.to_string());
                    }
                    _ => {}
                }
            }
            None
        })
        .unwrap_or(None)
}
#[cfg(not(feature = "skip-toolchain-check"))]
fn check_toolchain_version() {
    std::env::set_var("CLICOLOR_FORCE", "1"); // make sure our coloring gets through to cargo

    let current_hash = get_rustc_commit_hash().unwrap_or(String::from("<unknown>"));
    if current_hash != REQUIRED_COMMIT_HASH {
        eprintln!(
            "{}: {} (found {}). Make sure you specify {} in your project's {} file",
            "error".bright_red(),
            "Wrong toolchain detected".bold(),
            current_hash.bright_cyan(),
            format!("channel=\"{}\"", REQUIRED_TOOLCHAIN).bright_cyan(),
            "rust_toolchain".bright_cyan()
        );
        std::process::exit(1);
    }
}

#[cfg(feature = "skip-toolchain-check")]
fn check_toolchain_version() {}

fn main() {
    check_toolchain_version();
}
