use spirv_builder::SpirvBuilder;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // This will set the env var `compute_shader.spv` to a spir-v file that can be include!()'d
    SpirvBuilder::new("../../shaders/compute-shader").build()?;
    Ok(())
}
