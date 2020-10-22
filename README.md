# 🐉 Rust GPU

[![Contributor Covenant](https://img.shields.io/badge/contributor%20covenant-v1.4%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)
[![Embark](https://img.shields.io/badge/embark-open%20source-blueviolet.svg)](http://embark.dev)
[![Embark](https://img.shields.io/badge/discord%20chat-ark-green.svg)](https://discord.gg/dAuKfZS)

This is a very early stage project to make Rust a first-class language and ecosystem for building GPU code 🚀🚧

### Current Status: v0.1

Compiling and running very simple shaders works, and a significant portion of [the core library](https://doc.rust-lang.org/core/index.html) also compiles.

However, many things aren't implemented yet: for example, branching (e.g. if-statements) isn't supported yet! That means that while being technically usable, this project is far from being production-ready.

### Example

![Sky shader](assets/sky.jpg)

```rust
#[spirv(entry = "fragment")]
pub fn main_fs(input: Input<Vec4>, mut output: Output<Vec4>) {
    let dir: Vec3 = input.load().truncate();

    let cs_pos = Vec4(dir.0, -dir.1, 1.0, 1.0);
    let ws_pos = {
        let p = clip_to_world.mul_vec4(cs_pos);
        p.truncate() / p.3
    };
    let dir = (ws_pos - eye_pos).normalize();
    
    // evaluate Preetham sky model
    let color = sky(dir, sun_pos);

    output.store(color.extend(0.0))
}
```

See [source](examples/example-shader/src/lib.rs) for full details.

## Background

Historically in games GPU programming has been done through writing either HLSL, or to a lesser extent GLSL. These are simple programming languages that have evolved along with rendering APIs over the years. However, as game engines have evolved, these languages have failed to provide mechanisms for dealing with large codebases, and have generally stayed behind the curve compared to other programming languages.

In part this is because it's a niche language for a niche market, and in part this has been because the industry as a whole has sunk quite a lot of time and effort into the status quo. While over-all better alternatives to both languages exist, none of them are in a place to replace HLSL or GLSL. Either because they are vendor locked, or because they don't support the traditional graphics pipeline. Examples of this include CUDA and OpenCL. And while attempts have been made to create language in this space, none of them have gained any notable traction in the gamedev community.

Our hope with this project is that we push the industry forward by bringing an existing, low-level, safe, and high performance language to the GPU; namely Rust. And with it come some additional benefits that can't be overlooked: a package/module system that's one of the industry's best, built in safety against race-conditions or out of bounds memory access, a wide range of tools and utilities to improve programmer workflows, and many others!

### Why Embark?

At Embark, we've been building our in-house engine from the ground up in Rust. We have previous in-house experience developing [RLSL](https://github.com/MaikKlein/rlsl) (a first prototype of this idea) and we have some of the world's best rendering engineers that are familiar with the problems in current shading languages. So, we're in a unique place to solve this problem. We want to streamline our own internal development, facilitate code-sharing between GPU and CPU, but most importantly: to enable our users to very rapidly build great looking experiences. 

If we do this project right, one wouldn't necessarily need a team of rendering engineers to build a good looking game, instead one would simply use a few of the existing open-source crates that provide the graphical effects needed to create the experience you're after. Instead of sharing snippets of TAA code on forum posts, one could simply add the right crate(s).

## Project scope

This project will involve a few things if we want to get the experience right, and it's quite broad. Initial stages will involve mostly just setting up the backend, however the project will be broader then that.

- Currently only [SPIR-V](https://en.wikipedia.org/wiki/Standard_Portable_Intermediate_Representation) support is planned (the open compiler target for [Vulkan](https://en.wikipedia.org/wiki/Vulkan_(API)) but it's not unlikely that in future versions this will / should support [DXIL](https://github.com/microsoft/DirectXShaderCompiler/blob/master/docs/DXIL.rst) (the target for DirectX) or [WGSL](https://github.com/gpuweb/gpuweb/tree/main/wgsl) (the WebGPU shading language that's bijective with SPIR-V)
- The first and primary target will be Vulkan shaders (vertex, fragment, etc.). However, Vulkan compute (what SPIR-V calls GLCompute) is also on the roadmap, as well as OpenCL compute (what SPIR-V calls Kernel).
- Implement a `rustc` compiler backend, plugging in via `-Z codegen-backend`. This is the same mechanism that [rustc_codegen_cranelift](https://github.com/bjorn3/rustc_codegen_cranelift) and [rustc_codegen_gcc](https://github.com/antoyo/rustc_codegen_gcc) use.
- [crates.io](https://crates.io) support to be able to publish SPIR-V crates
- An Embark-provided rendering / framegraph abstraction to take advantage of this and to make it easy for users to re-use rendering effects.

An in-depth exploration of our roadmap and milestones can be found [here](https://github.com/EmbarkStudios/rust-gpu/issues/47).

## Process

We use this repo as a monorepo for everything related to the project: crates, tools, shaders, examples, tests, and design documents. This way, we can use issues and PRs covering everything in the same place, cross-reference stuff within the repo, as well as with other GitHub repos such as [rspirv](https://github.com/gfx-rs/rspirv) and [Rust](https://github.com/rust-lang/rust) itself.

We meet weekly over a Discord call to discuss design and triage issues. Each meeting has an [issue](https://github.com/EmbarkStudios/rust-gpu/issues?q=label%3Ameeting+) with agenda, links and minutes.

We have a [#rust-gpu Discord channel](https://discord.gg/dAuKfZS) for fast discussion and collaboration.

## Structure

There are a few different components to this repo:

* [rfcs](rfcs) for in-depth discussion and specs.
* [rustc_codegen_spirv](rustc_codegen_spirv) for the compiler itself.
* [spirv-std](spirv-std) for GPU intrinsics, types, and other library items used by GPU crates.
* [spirv-builder](spirv-builder) for a convenient way of building a GPU crate in a CPU build.rs file.

## Getting started

1. Clone the repository.

1. Install the prerequisites using the provided setup script. From the root of the project, run:

    MacOS, Linux:
    ```shell
    sh setup.sh
    ```
    
    Windows:
    ```
    setup.bat
    ```
    
    The setup script installs nightly Rust (required for now, see [#78](https://github.com/EmbarkStudios/rust-gpu/issues/78) for tracking issue).

1. Install [SPIRV-Tools](https://github.com/KhronosGroup/SPIRV-Tools#downloads) and add it to your PATH (for now, eventually we will automatically build and link it instead of calling executables)

1. Next, look at the [examples](examples) folder. There are two projects here: [examples/example-shader](examples/example-shader) and [examples/example-runner](examples/example-runner). The example-shader project is a "GPU crate", one that will be compiled to a spir-v module. The example-runner project is a normal, CPU crate that uses vulkan to consume the example-shader spir-v module to display a "hello world" triangle.
    
    Run the example:

    ```shell
    cargo run --bin example-runner
    ```
    
    This will build `rustc_codegen_spirv`, the compiler, then use that compiler to build `example-shader` into a SPIR-V module, then finally, build a vulkan sample app (taken from [ash's examples](https://github.com/MaikKlein/ash/blob/master/examples/src/bin/triangle.rs)) using the built SPIR-V module to display a triangle in a window.

    All of this is orchestrated by the [spirv-builder](spirv-builder) crate, which is used in example-runner's `build.rs` file. Please look at that file, as well as both example projects in general, to see how to set up your own shaders!

Be aware that this project is in a very early phase - if the above doesn't work, please [file an issue](https://github.com/EmbarkStudios/rust-gpu/issues)!

## Getting started, for power users who don't want to use spirv-builder.

If you would like to build the compiler, `rustc_codegen_spirv` is the relevant folder. Install the prerequisites, as above, then, `cd rustc_codegen_spirv && cargo build`. This produces an .so file, located at `./target/debug/librustc_codegen_spirv.so` (or `.dll`/`.dylib` depending on your platform).

This file is a dynamically loaded backend for rustc - you may tell rustc to use it as a backend through the `-Z codegen-backend=...` flag. To pass this to rustc through cargo, set the environment variable `RUSTFLAGS="-Z codegen-backend=$PATH_TO_FILE"`.

Then, when building a GPU crate, we need to configure some flags when we call cargo. First, we need to build libcore
ourselves - we obviously have no SPIR-V libcore installed on our system! Use the flag `-Z build-std=core`. Then, we need
to tell rustc to generate SPIR-V instead of x86 code: `--target spirv-unknown-unknown`.

Overall, building your own SPIR-V crate looks like:

```shell
export RUSTFLAGS="-Zcodegen-backend=$THIS_REPO/target/debug/librustc_codegen_spirv.so"
cargo build -Z build-std=core --target spirv-unknown-unknown --release
```

(with an appropriate path for `$THIS_REPO`, and replacing `export` with `set` if you're on windows as well as the proper dll name)

This will produce a `target/spirv-unknown-unknown/release/crate_name.spv` file.

To create a GPU crate, look at the [examples/example-shader](examples/example-shader) crate. In short, reference the `spirv-std` crate, and use intrinsics defined there to create your shader.

This is all a little convoluted, hence the [spirv-builder](spirv-builder) crate handles a lot of this.

## Contributing

We welcome community contributions to this project.

Please read our [Contributor Guide](CONTRIBUTING.md) for more information on how to get started.

## License

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
