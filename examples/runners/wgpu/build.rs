use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS")?;
    let target_arch = std::env::var("CARGO_CFG_TARGET_ARCH")?;
    // Always build on CI to make sure the shaders can still build
    let is_on_ci = std::env::var("CI");
    if target_os != "android" && target_arch != "wasm32" && is_on_ci.is_err() {
        return Ok(());
    }
    let status = std::process::Command::new("cargo")
        .args(["run", "--release", "-p", "example-runner-wgpu-builder"])
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()?;
    if !status.success() {
        if let Some(code) = status.code() {
            std::process::exit(code);
        } else {
            std::process::exit(1);
        }
    }
    Ok(())
}
