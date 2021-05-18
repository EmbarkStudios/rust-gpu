use spirv_builder::SpirvBuilder;
use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

fn build_shader(path_to_create: &str, codegen_names: bool) -> Result<(), Box<dyn Error>> {
    let result = SpirvBuilder::new(path_to_create, "spirv-unknown-vulkan1.0").build()?;
    if codegen_names {
        let out_dir = env::var_os("OUT_DIR").unwrap();
        let dest_path = Path::new(&out_dir).join("entry_points.rs");
        fs::write(&dest_path, result.codegen_entry_point_strings()).unwrap();
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    build_shader("../../shaders/sky-shader", true)?;
    build_shader("../../shaders/simplest-shader", false)?;
    build_shader("../../shaders/compute-shader", false)?;
    build_shader("../../shaders/mouse-shader", false)?;
    Ok(())
}
