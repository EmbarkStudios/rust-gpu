use spirv_builder::SpirvBuilder;
use std::error::Error;
use std::path::Path;
use std::{env, fs};

fn main() -> Result<(), Box<dyn Error>> {
    println!("cargo:rerun-if-changed=build.rs");
    // While OUT_DIR is set for both build.rs and compiling the crate, PROFILE
    // is only set in build.rs. So, export it to crate compilation as well.
    println!(
        "cargo:rustc-env=PROFILE={}",
        std::env::var("PROFILE").unwrap()
    );

    for path_to_crate in [
        "../../shaders/sky-shader",
        "../../shaders/simplest-shader",
        "../../shaders/compute-shader",
        "../../shaders/mouse-shader",
    ] {
        let result = SpirvBuilder::new(path_to_crate, "spirv-unknown-vulkan1.1").build()?;
        if path_to_crate.ends_with("/sky-shader") {
            let out_dir = env::var_os("OUT_DIR").unwrap();
            let dest_path = Path::new(&out_dir).join("entry_points.rs");
            fs::create_dir_all(&out_dir).unwrap();
            fs::write(dest_path, result.codegen_entry_point_strings()).unwrap();
        }
    }
    Ok(())
}
