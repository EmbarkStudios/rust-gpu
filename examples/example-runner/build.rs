use spirv_builder::SpirvBuilder;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // This will set the env var `example-shader.spv` to a spir-v file that can be include!()'d
    SpirvBuilder::new("../example-shader").build()?;
    Ok(())
}
