use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // This will set the env var `wgpu-example-shader.spv` to a spir-v file that can be include!()'d
    #[cfg(feature = "build-spirv")]
    spirv_builder::SpirvBuilder::new(".")
        .spirv_version(1, 0)
        .build()?;
    Ok(())
}
