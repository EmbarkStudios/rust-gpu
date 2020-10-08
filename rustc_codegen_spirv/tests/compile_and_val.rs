use std::path::Path;
use std::process::Command;

#[test]
pub fn build_libcore_test() {
    {
        // Unfortunately, as of right now, cargo does not rebuild projects if the .so provided by
        // codegen-backend has changed. So, force a full rebuild always by deleting the target dir.
        let target_dir = Path::new("build_libcore_test/target");
        if target_dir.exists() {
            std::fs::remove_dir_all(target_dir).unwrap();
        }
    }
    let rustflags = format!(
        "-Z codegen-backend={}rustc_codegen_spirv{}",
        std::env::consts::DLL_PREFIX,
        std::env::consts::DLL_SUFFIX
    );
    let build = Command::new("cargo")
        .args(&[
            "build",
            "-Z",
            "build-std=core",
            "--target",
            "spirv-unknown-unknown",
            "--release",
        ])
        .current_dir(Path::new("build_libcore_test").canonicalize().unwrap())
        .env("RUSTFLAGS", rustflags)
        .env("SPIRV_VAL", "1")
        .status()
        .expect("failed to execute cargo build");
    if !build.success() {
        panic!("build_libcore_test compilation failed with code {}", build);
    }
}
