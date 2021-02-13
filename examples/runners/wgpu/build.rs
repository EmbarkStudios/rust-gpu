fn main() -> Result<(), Box<dyn std::error::Error>> {
    // build.rs target_os and target_arch is the compiling computer, not the target so #[cfg] won't work
    let target_os = std::env::var("CARGO_CFG_TARGET_OS")?;
    let target_arch = std::env::var("CARGO_CFG_TARGET_ARCH")?;
    if target_os != "android" && target_arch != "wasm32" {
        return Ok(());
    }
    use spirv_builder::SpirvBuilder;
    use std::error::Error;

    fn build_shader(path_to_create: &str) -> Result<(), Box<dyn Error>> {
        SpirvBuilder::new(path_to_create)
            .spirv_version(1, 0)
            .build()?;
        Ok(())
    }

    build_shader("../../shaders/sky-shader")?;
    build_shader("../../shaders/simplest-shader")?;
    build_shader("../../shaders/compute-shader")?;
    build_shader("../../shaders/mouse-shader")?;
    Ok(())
}
