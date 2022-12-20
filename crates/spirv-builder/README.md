<!-- inline html -->
<!-- markdownlint-disable-file MD033 -->
# `spirv-builder`

![Rust version](https://img.shields.io/badge/rust-nightly--2022--10--29-purple.svg)

This crate gives you `SpirvBuilder`, a tool to build shaders using [rust-gpu][rustgpu].

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

## Building with `spirv-builder`

Because of its nature, `rustc_codegen_spirv`, and therefore `spirv-builder` by extension, require the use of a very specific nightly toolchain of Rust.

**The current toolchain is: `nightly-2022-10-29`.**

Toolchains for previous versions of `spirv-builder`:

|Version|Toolchain|
|-:|-|
|`0.4.0`|`nightly-2022-10-29`|
|`0.4.0-alpha.16` - `0.4.0-alpha.17`|`nightly-2022-10-01`|
|`0.4.0-alpha.15`|`nightly-2022-08-29`|
|`0.4.0-alpha.13` - `0.4.0-alpha.14`|`nightly-2022-04-11`|

The nightly toolchain has to match *exactly*. Starting with `0.4.0-alpha.15`, the commit hash of your local toolchain is checked and you'll get a build error when building `rustc_codegen_spirv` with the wrong toolchain. If you want to experiment with different versions, this check can be omitted by defining the environment variable `RUSTGPU_SKIP_TOOLCHAIN_CHECK`<sup>since `0.4.0-alpha.16`</sup>. Keep in mind that, as `rustc_codegen_spirv` is heavily dependent on `rustc`'s internal API, diverging too much from the required toolchain will quickly result in compile errors.

[rustgpu]: https://github.com/EmbarkStudios/rust-gpu/
