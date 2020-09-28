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
        "-Z codegen-backend=librustc_codegen_spirv{} -C codegen-units=1",
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
        .status()
        .expect("failed to execute cargo build");
    if !build.success() {
        panic!("build_libcore_test compilation failed with code {}", build);
    }

    let output_path =
        Path::new("build_libcore_test/target/spirv-unknown-unknown/release/build_libcore_test.spv");
    let val = std::process::Command::new("spirv-val")
        .arg(&output_path)
        .status()
        .expect("failed to execute spirv-val");
    if !val.success() {
        panic!("build_libcore_test validation failed with code {}", build);
    }
}
