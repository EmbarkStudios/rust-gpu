use std::error::Error;
use std::path::PathBuf;

use spirv_builder::{options::{Source, SourceKind}, SpirvBuilder};

fn main() -> Result<(), Box<dyn Error>> {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR")?);
    // This will set the env var `sky-shader.spv` to a spir-v file that can be include!()'d
    SpirvBuilder::new(
        "../../shaders/sky-shader",
        std::env::var("OUT_DIR").unwrap(),
    )
    .spirv_version(1, 3)
    .codegen_source(Source {
        compile_source: true,
        kind: SourceKind::Path(std::fs::canonicalize("../../../")?)
    })
    .sysroot_location(Source {
        compile_source: true,
        kind: SourceKind::Path(out_dir.join("sysroot"))
    })
    .build()?;
    Ok(())
}
