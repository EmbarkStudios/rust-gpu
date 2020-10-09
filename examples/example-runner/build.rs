use spirv_builder::build_spirv;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // This will set the env var `example-shader.spv` to a spir-v file that can be include!()'d
    build_spirv("../../rustc_codegen_spirv", "../example-shader")?;
    Ok(())
}
