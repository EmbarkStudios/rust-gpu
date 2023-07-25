<!-- inline html -->
<!-- markdownlint-disable-file MD033 -->
# `spirv-builder`

![Rust version](https://img.shields.io/badge/rust-nightly--2023--05--27-purple.svg)

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

As `spirv-builder` relies on `rustc_codegen_spirv` being built for it (by Cargo, as a direct dependency), and due to the special nature of the latter (as a `rustc` codegen backend "plugin"), both end up sharing the requirement for a very specific nightly toolchain version of Rust.

**The current Rust toolchain version is: `nightly-2023-05-27`.**

Rust toolchain version history across [rust-gpu releases](https://github.com/EmbarkStudios/rust-gpu/releases) (since `0.4`):

|`spirv-builder`<br>version|Rust toolchain<br>version|
|:-:|:-:|
|`0.9`|`nightly-2023-05-27`|
|`0.8`|`nightly-2023-04-15`|
|`0.7`|`nightly-2023-03-04`|
|`0.6`|`nightly-2023-01-21`|
|`0.5`|`nightly-2022-12-18`|
|`0.4`|`nightly-2022-10-29`|

<sup>*As patch versions must be semver-compatible, they will always require the  
same toolchain (for example, `0.6.0` and `0.6.1` both use `nightly-2023-01-21`).*</sup>

Only that *exact* Rust nightly toolchain version is **supported**. Since `0.4`, the commit hash of your current Rust toolchain is checked and you'll get a build error when building `rustc_codegen_spirv` with the wrong toolchain.  
Notably, the error will also show what the `rust-toolchain.toml` file *should* contain (to get the expected toolchain), which you can rely on when updating to a new release.

If you want to experiment with _different, **unsupported**_, Rust toolchain versions, this check can be omitted by defining the environment variable `RUSTGPU_SKIP_TOOLCHAIN_CHECK`. Keep in mind that, as `rustc_codegen_spirv` is *heavily* dependent on `rustc`'s internal APIs, diverging too much from the supported toolchain version will quickly result in compile errors (or worse, e.g. spurious errors and/or incorrect behavior, when compiling shaders with it).

[rustgpu]: https://github.com/EmbarkStudios/rust-gpu/
