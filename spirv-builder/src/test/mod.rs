mod basic;

use lazy_static::lazy_static;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

// Tests need to run serially, since they write project files to disk and whatnot. We don't want to
// create a new temp dir for every test, though, since then every test would need to build libcore.
// We could require the user to pass --thread-count 1 to cargo test, but that affects other tests.
// So make a global mutex wrapping every test.
lazy_static! {
    static ref GLOBAL_MUTEX: Mutex<()> = Mutex::new(());
}

static CARGO_TOML: &str = r#"[package]
name = "test-project"
version = "0.1.0"
authors = ["Embark <opensource@embark-studios.com>"]
edition = "2018"

[lib]
crate-type = ["dylib"]

[dependencies]
spirv-std = { path = "../../spirv-std" }

[workspace]
"#;

static SRC_PREFIX: &str = r#"#![no_std]
#![feature(lang_items, register_attr)]
#![register_attr(spirv)]
use core::panic::PanicInfo;
use spirv_std::*;
#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}
"#;

fn setup(src: &str) -> Result<PathBuf, Box<dyn Error>> {
    let project = Path::new("../target/test-spirv").to_owned();
    let cargo_toml = project.join("Cargo.toml");
    let lib_rs = project.join("src/lib.rs");
    std::fs::create_dir_all(lib_rs.parent().unwrap())?;
    // don't write cargo.toml if unchanged, so it doesn't get deps refreshed
    if std::fs::read(&cargo_toml).map_or(true, |b| b != CARGO_TOML.as_bytes()) {
        std::fs::write(cargo_toml, CARGO_TOML)?;
    }
    std::fs::write(lib_rs, format!("{}{}", SRC_PREFIX, src))?;
    Ok(project)
}

fn build(src: &str) -> PathBuf {
    let project = setup(src).expect("Failed to set up project");
    crate::SpirvBuilder::new(&project)
        .print_metadata(false)
        .build()
        .expect("Failed to build test")
}

fn val(src: &str) {
    let _lock = GLOBAL_MUTEX.lock().unwrap();
    // spirv-val is included in building
    build(src);
}
