use spirv_builder::SpirvBuilder;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // This will set the env var `wgpu-example-shader.spv` to a spir-v file that can be include!()'d
    SpirvBuilder::new("../wgpu-example-shader").build()?;
    Ok(())
}
