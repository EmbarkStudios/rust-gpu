use spirv_builder::SpirvBuilder;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    if std::env::var("CARGO_FEATURE_NO_SPIRV_BUILD").is_ok() {
        return Ok(());
    }

    // This will set the env var `example-shader.spv` to a spir-v file that can be include!()'d
    SpirvBuilder::new(".")
        .spirv_version(1, 0)
        .build()?;
    Ok(())
}
