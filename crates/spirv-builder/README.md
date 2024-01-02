<!-- inline html -->
<!-- markdownlint-disable-file MD033 -->
# `spirv-builder`

<!-- FIXME(eddyb) this should maybe be moved to `spirv-builder` docs? -->

This crate provides `SpirvBuilder`, a tool to build shaders using [rust-gpu][rustgpu].

It takes care of pulling in the `SPIR-V` backend for Rust, `rustc_codegen_spirv`, and invoking a nested build using appropriate compiler options, some of which may be set using the `SpirvBuilder` API.

## Example

```rust
use spirv_builder::{MetadataPrintout, SpirvBuilder};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    SpirvBuilder::new("my_shaders", "spirv-unknown-vulkan1.1")
        .print_metadata(MetadataPrintout::Full)
        .build()?;
    Ok(())
}
```

This example will build a shader crate called `my_shaders`. You typically insert this code in your crate's `build.rs` that requires the shader binary. The path to the shader module's binary will be set in the `my_shaders.spv` environment variable, which you can include in your project using something along the lines of:

```rust
const SHADER: &[u8] = include_bytes!(env!("my_shaders.spv"));
```

[rustgpu]: https://github.com/EmbarkStudios/rust-gpu/
