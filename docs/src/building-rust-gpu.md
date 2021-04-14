# Building Rust-GPU

## Getting started
1. Clone the repository.

    ```shell
    git clone --recurse-submodules https://github.com/EmbarkStudios/rust-gpu
    ```

1. **optional** Install [SPIRV-Tools](https://github.com/KhronosGroup/SPIRV-Tools#downloads) and add it to your `PATH`. You can skip this step if you just want to run examples with the defaults. See [Using installed SPIRV-Tools](#using-installed-spirv-tools) if you decide to go with this option.

1. Next, look at the [examples] folder. There are two kinds of targets here: [runners] and [shaders]. The projects inside `shaders` are "GPU crates", i.e. ones that will be compiled to one or more SPIR-V modules. The `runner` projects are normal, CPU crates that use some graphics backend (currently, we have a [`wgpu` runner][examples/runners/wgpu], a [Vulkan runner][examples/runners/ash] through `ash`, and a barebones pure software [CPU runner][examples/runners/cpu]) to actually run one of the "GPU crate" shaders.

    Run the example:

    ```shell
    cargo run --bin example-runner-wgpu
    ```

    This will build `rustc_codegen_spirv`, the compiler, then use that compiler to build [`sky-shader`](examples/shaders/sky-shader) into a SPIR-V module, then finally, build a `wgpu` sample app (modified from [`wgpu`'s examples](https://github.com/gfx-rs/wgpu-rs/tree/master/examples/hello-triangle)) using the built SPIR-V module to display the shader in a window.

## Using installed SPIRV-Tools

By default, all of the crates and examples in this repo will compile the [`spirv-tools-sys`](https://crates.io/crates/spirv-tools-sys) crate, including a lot of C++ code from [SPIRV-Tools](https://github.com/EmbarkStudios/SPIRV-Tools). If you don't want to build the C++ code because you already have [SPIRV-Tools](https://github.com/KhronosGroup/SPIRV-Tools#downloads) installed, or just don't want to spend more time compiling, you can build/run the crate with the `use-installed-tools` feature.

```shell
cargo run \
    --manifest-path examples/example-runner/Cargo.toml \
    --features use-installed-tools \
    --no-default-features
```

You should see `warning: use-installed-tools feature on, skipping compilation of C++ code` during the compilation, but otherwise the build will function just the same as if you compiled the C++ code, with the exception that it will fail if you don't have SPIRV-Tools installed correctly.

[spirv-builder]: https://embarkstudios.github.io/rust-gpu/api/spirv_builder/index.html
[examples]: https://github.com/EmbarkStudios/rust-gpu/tree/main/examples
[examples/runners]: https://github.com/EmbarkStudios/rust-gpu/tree/main/examples/runners
[examples/runners/ash]: https://github.com/EmbarkStudios/rust-gpu/tree/main/examples/runners/ash
[examples/runners/cpu]: https://github.com/EmbarkStudios/rust-gpu/tree/main/examples/runners/cpu
[examples/runners/wgpu]: https://github.com/EmbarkStudios/rust-gpu/tree/main/examples/runners/wgpu
[examples/shaders]: https://github.com/EmbarkStudios/rust-gpu/tree/main/examples/shaders
