mod basic;

use lazy_static::lazy_static;
use rustc_codegen_spirv::rspirv;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, MutexGuard};

// https://github.com/colin-kiegel/rust-pretty-assertions/issues/24
#[derive(PartialEq, Eq)]
pub struct PrettyString<'a>(pub &'a str);
/// Make diff to display string as multi-line string
impl<'a> std::fmt::Debug for PrettyString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

// Tests need to run serially, since they write project files to disk and whatnot. We don't want to
// create a new temp dir for every test, though, since then every test would need to build libcore.
// We could require the user to pass --thread-count 1 to cargo test, but that affects other tests.
// So make a global mutex wrapping every test.
lazy_static! {
    static ref GLOBAL_MUTEX: Mutex<()> = Mutex::new(());
}

fn global_lock() -> MutexGuard<'static, ()> {
    match GLOBAL_MUTEX.lock() {
        Ok(guard) => guard,
        // we don't care about poison - a previous test may have paniced, but that's okay
        Err(poisoned) => poisoned.into_inner(),
    }
}

static CARGO_TOML: &str = r#"[package]
name = "test-project"
version = "0.1.0"
authors = ["Embark <opensource@embark-studios.com>"]
edition = "2018"

[lib]
crate-type = ["dylib"]

[profile.dev]
overflow-checks = false
debug-assertions = false

[dependencies]
spirv-std = { path = "../../crates/spirv-std", features=["const-generics"] }
glam = { git = "https://github.com/EmbarkStudios/glam-rs.git", rev="7476a96", default-features=false, features = ["libm", "scalar-math"] }

[patch.crates-io.spirv-std]
path="../../crates/spirv-std"

[workspace]
"#;

static SRC_PREFIX: &str = r#"#![no_std]
#![feature(register_attr, asm, ptr_internals)]
#![register_attr(spirv)]
#![deny(warnings)]

#[allow(unused_imports)]
use spirv_std::{*, num_traits::Float as _ };
"#;

fn setup(src: &str) -> Result<PathBuf, Box<dyn Error>> {
    let project = Path::new("../../target/test-spirv").to_owned();
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
    crate::SpirvBuilder::new(&project, "spirv-unknown-spv1.3")
        .print_metadata(false)
        .release(false)
        .build()
        .expect("Failed to build test")
}

fn read_module(path: &Path) -> Result<rspirv::dr::Module, Box<dyn Error>> {
    let bytes = std::fs::read(path)?;
    let mut loader = rspirv::dr::Loader::new();
    rspirv::binary::parse_bytes(&bytes, &mut loader)?;
    Ok(loader.module())
}

fn val(src: &str) {
    let _lock = global_lock();
    // spirv-val is included in building
    build(src);
}

fn assert_str_eq(expected: &str, result: &str) {
    let expected = expected
        .split('\n')
        .map(|l| l.trim())
        .collect::<Vec<_>>()
        .join("\n");

    let result = result
        .split('\n')
        .map(|l| l.trim().replace("  ", " ")) // rspirv outputs multiple spaces between operands
        .collect::<Vec<_>>()
        .join("\n");

    pretty_assertions::assert_eq!(PrettyString(&expected), PrettyString(&result))
}

fn dis_fn(src: &str, func: &str, expect: &str) {
    let _lock = global_lock();
    let module = read_module(&build(src)).unwrap();
    let abs_func_path = format!("test_project::{}", func);
    let id = module
        .debugs
        .iter()
        .find(|inst| {
            inst.class.opcode == rspirv::spirv::Op::Name
                && inst.operands[1].unwrap_literal_string() == abs_func_path
        })
        .unwrap_or_else(|| {
            panic!(
                "no function with the name `{}` found in:\n{}\n",
                abs_func_path,
                module.disassemble()
            )
        })
        .operands[0]
        .unwrap_id_ref();
    let mut func = module
        .functions
        .into_iter()
        .find(|f| f.def_id().unwrap() == id)
        .unwrap();
    // Compact to make IDs more stable
    compact_ids(&mut func);
    use rspirv::binary::Disassemble;
    assert_str_eq(expect, &func.disassemble())
}

fn dis_entry_fn(src: &str, func: &str, expect: &str) {
    let _lock = global_lock();
    let module = read_module(&build(src)).unwrap();
    let id = module
        .entry_points
        .iter()
        .find(|inst| inst.operands.last().unwrap().unwrap_literal_string() == func)
        .unwrap_or_else(|| {
            panic!(
                "no entry point with the name `{}` found in:\n{}\n",
                func,
                module.disassemble()
            )
        })
        .operands[1]
        .unwrap_id_ref();
    let mut func = module
        .functions
        .into_iter()
        .find(|f| f.def_id().unwrap() == id)
        .unwrap();
    // Compact to make IDs more stable
    compact_ids(&mut func);
    use rspirv::binary::Disassemble;
    assert_str_eq(expect, &func.disassemble())
}

fn dis_globals(src: &str, expect: &str) {
    let _lock = global_lock();
    let module = read_module(&build(src)).unwrap();

    use rspirv::binary::Disassemble;
    let dis = module
        .global_inst_iter()
        .map(|inst| inst.disassemble())
        .collect::<Vec<String>>()
        .join("\n");
    assert_str_eq(expect, &dis);
}

fn compact_ids(module: &mut rspirv::dr::Function) -> u32 {
    let mut remap = std::collections::HashMap::new();
    let mut insert = |current_id: &mut u32| {
        let len = remap.len();
        *current_id = *remap.entry(*current_id).or_insert_with(|| len as u32 + 1)
    };
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(ref mut result_id) = &mut inst.result_id {
            insert(result_id)
        }
        if let Some(ref mut result_type) = &mut inst.result_type {
            insert(result_type)
        }
        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = op.id_ref_any_mut() {
                insert(w)
            }
        })
    });
    remap.len() as u32 + 1
}
