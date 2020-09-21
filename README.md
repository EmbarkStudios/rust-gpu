# 🐉 Rust GPU

[![Contributor Covenant](https://img.shields.io/badge/contributor%20covenant-v1.4%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)
[![Embark](https://img.shields.io/badge/embark-open%20source-blueviolet.svg)](http://embark.dev)
[![Embark](https://img.shields.io/badge/discord%20chat-ark-green.svg)](https://discord.gg/dAuKfZS)

This is a very early stage project to make Rust a first-class language and ecosystem for building GPU code 🚀🚧

## Background

Historically in games GPU programming has been done through writing either HLSL, or to a lesser extent GLSL. These are simple programming languages that have evolved along with rendering APIs over the years. However, as game engines have evolved, these languages have failed to provide mechanisms for dealing with large codebases, and have generally stayed behind the curve compared to other programming languages.

In part this is because it's a niche language for a niche market, and in part this has been because the industry as a whole has sunk quite a lot of time and effort into the status quo. While over-all better alternatives to both languages exist, none of them are in a place to replace HLSL or GLSL. Either because they're vendor locked, or because they don't suppor the traditional graphics pipeline. Examples of this include CUDA and OpenCL. And while attempts have been made to create language in this space, none of them have gained any notable traction in the gamedev community.

Our hope with this project is that we push the industry forward by bringing an existing low-level, safe and high performance language to the GPU; that language being Rust. And with it come some additional benefits that can't be overlooked: a package/module system that's one of the industry's best, built in safety against race-conditions or out of bounds memory access etc. 

## Why Embark?

At Embark, we've been building our in-house engine from the ground up in Rust and we have some previous in-house experience developing [RLSL](https://github.com/MaikKlein/rlsl) (a first prototype of this idea) and we have some of the world's best rendering engineers that are familiar with the problems in current shading languages so we're in a unique place to solve this problem. To streamline our own internal development, facilitate code-sharing between GPU and CPU, but most importantly: to enable our users to very rapidly build great looking experiences. If we do this project right one wouldn't nessisarily need a team of rendering engineers to build a good looking game, instead one would simply use a few of the existing open-source crates that provide the graphical effects needed to create the experience you're after. Instead of sharing snippets of TAA code on forum posts one could simply add the right crate(s).

## Project scope

This project will involve a few things if we want to get the experience right and it's quite broad. Initial stages will involve mostly just setting up the backend, however the project will be a bit broader then that.

- A `rustc` compiler backend, either as a cranelift module, or as a seperate `rustc` backend (next to llvm and cranelift). We're currently evaluating both options, but it looks like a `rustc` native backend would be the most preferable.
- This compiler backend is currently planned to only support SPIR-V (the open compiler target for Vulkan) but it's not unlikely that in future versions this will / should support DXIL (the target for DirectX) or WHLSL (the WebGPU shading language that's bijective with SPIR-V)
- We'll need language front-end features to be able to support GPU workloads better; this will include among other things
  - Safe access to groupshared and constant memory
  - Support for intrinsics
  - Support for resource binding
- [crates.io](https://crates.io) support to be able to publish SPIR-V crates
- An Embark-provided rendering / framegraph abstraction to take advantage of this and to make it easy for users to re-use rendering effects.

The rustc compiler backend support levels are indicated by tiers; macOS, Linux and desktop PC are all Tier 1. However "smaller" platforms such as Android and iOS are Tier 2 and even smaller projects are Tier 3.

I think our initial goal should be to replace our own internal shaders with shaders written in Rust, and then to achieve Tier 3 support, which roughly means that it's possible to get up and running if you're willing to put in some effort into it. Usually this involves setting up a custom build environment, and getting it running in a custom environment. However, I think the ambition of the project ultimately should be Tier 1 support.

## Process

We use this repo as a small monorepo for everything related to the project: crates, tools, shaders, examples, tests, and design documents. This way we can use issues and PRs covering everything in the same place cross-reference stuff within the repo as well as with other GitHub repos (Cranelift/Rust/Ark).

We meet weekly over vidcon to discuss design and triage issues. Each meeting has an [issue](https://github.com/EmbarkStudios/rust-gpu/issues?q=label%3Ameeting+) with agenda, links and minutes.

We have a [#rust-gpu Discord channel](https://discord.gg/dAuKfZS) for fast discussion and collaboration. Once you join the Discord, ping @repi for access if you don't have it already.

For discussions with [Bytecode Alliance](https://bytecodealliance.org/) and Cranelift developers, there is a public [Zulip chat thread](https://bytecodealliance.zulipchat.com/#narrow/stream/225524-cranelift-new-backend/topic/spir-v)

## Getting started

There are a few different components to this repo - for example, the [rfcs folder](rfcs) is for in-depth discussion and specs. If you would like to build the compiler, `rustc_codegen_spirv` is the relevant folder.

1) Install the prerequisites (The `setup.sh`/`setup.bat` scripts automate this):
    ```shell
    rustup install nightly
    rustup +nightly component add rust-src rustc-dev llvm-tools-preview
    ```
2) `cargo build`. Note the `rust-toolchain` file that specifies nightly, this is equivalent to passing `cargo +nightly build`, without having to type that out. (If you run that `rustup component add` in this directory, you don't need to pass +nightly either)
3) Run (i.e. compile a shader): This is pretty complicated right now, we're working on making the UX better. It involves passing `-Z codegen-backend` to rustc, and `-Z build-std=core` and `--target spirv-unknown-unknown` to cargo. However, both building the compiler, and compiling a sample test project (`build_libcore_test`) is automated by the script `build_libcore_test.sh`/`build_libcore_test.bat`. Feel free to inspect those scripts for what's needed, if you'd like to try yourself.

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
