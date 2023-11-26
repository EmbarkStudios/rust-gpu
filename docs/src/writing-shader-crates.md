# Writing Shader Crates

This is section is going to walk you through writing a shader in Rust and
setting up your shader crate.

Be aware that this project is in a very early phase, please [file an
issue](https://github.com/EmbarkStudios/rust-gpu/issues) if there's something
not working or unclear.

## Online

You can now test out and try building shaders with rust-gpu from the browser!

- [SHADERed] A shader IDE which has a lite version, which allows you to build
  and run shaders on the web.
- [Shader Playground] A playground for building and checking the output of
  shader code similar to godbolt or play.rust-lang.org.

[SHADERed]: https://shadered.org/template
[shader playground]: http://shader-playground.timjones.io/9d744d5893beb6a8f129fda50ad4aeeb

## Using `spirv-builder`

The `spirv-builder` crate automates building everything necessary behind the
scenes (with the right `rustup` toolchain etc.) to be able to compile SPIR-V
shaders from your own Rust crates, with a convenient API.

Adding a build script is the simplest way to use `spirv-builder`:
1. Reference `spirv-builder` in your Cargo.toml:
    ```toml
    [build-dependencies]
    spirv-builder = "0.9"
    ```
2. Create a `build.rs` in your project root.

### `build.rs`
Paste the following into `build.rs`

```rust,no_run
use spirv_builder::{MetadataPrintout, SpirvBuilder};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    SpirvBuilder::new(shader_crate, target)
        .print_metadata(MetadataPrintout::Full)
        .build()?;
    Ok(())
}
```

Substituting `shader_crate` with a relative path to your shader crate. The values available for the `target` parameter are available
[here](./platform-support.md).  For example, if building for vulkan 1.1, use
`"spirv-unknown-vulkan1.1"`.

The `SpirvBuilder` struct has numerous configuration options available, see
[documentation](https://embarkstudios.github.io/rust-gpu/api/spirv_builder/struct.SpirvBuilder.html).

### `main.rs`
The following will directly include the shader module binary into your application.
```rust,no_run
const SHADER: &[u8] = include_bytes!(env!("<shader_crate>.spv"));
```

> **Note** If your shader name contains hyphens, the name of environment variable will be the name with hyphens changed to underscores.

## Writing your first shader

Configure your shader crate as a `"dylib"` type crate, and add `spirv-std` to its dependencies:

```toml
[lib]
# FIXME(eddyb) this should suggest at least `["lib", "dylib"]`, but also
# `"dylib"` isn't ideal, we should be doing `staticlib` or even `bin`.
crate-type = ["dylib"]

[dependencies]
spirv-std = "0.9"
```

Make sure your shader code uses `#![no_std]` and imports the `spirv` attribute from `spirv-std`. Then, you're ready to write your first shader. Here's a very simple fragment shader called `main_fs` as an example that outputs the color red:

```rust,norun
#![no_std]

use spirv_std::spirv;
use spirv_std::glam::{vec4, Vec4};

#[spirv(fragment)]
pub fn main_fs(output: &mut Vec4) {
    *output = vec4(1.0, 0.0, 0.0, 1.0);
}
```
