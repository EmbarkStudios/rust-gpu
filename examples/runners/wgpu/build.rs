use std::error::Error;
use std::path::PathBuf;

use spirv_builder::{options::{Source, SourceKind}, SpirvBuilder};

fn build_shader(path_to_crate: &str) -> Result<(), Box<dyn Error>> {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR")?);
    SpirvBuilder::new(path_to_crate, &out_dir)
        .spirv_version(1, 3)
        .codegen_source(Source {
            compile_source: true,
            kind: SourceKind::Path("../../../".into())
        })
        .sysroot_location(Source {
            compile_source: true,
            kind: SourceKind::Path(out_dir.join("sysroot"))
        })
        .emit_build_script_metadata()
        .build()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    build_shader("../../shaders")?;
    Ok(())
}
