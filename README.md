<!-- Allow this file to not have a first line heading -->
<!-- markdownlint-disable-file MD041 -->
<!-- Disable warning om emphasis after first heading -->
<!-- markdownlint-disable-file MD036 -->

<!-- inline html -->
<!-- markdownlint-disable-file MD033 -->

<div align="center">

# `🐉 rust-gpu`

**Rust as a first-class language and ecosystem for GPU graphics & compute shaders**

## This project has moved home!

This repository was the nursery of `rust-gpu` during its inception and while it was still under the
stewardship of [Embark Studios](https://embark.dev/). It has now moved into community ownership
under the Rust-GPU GitHub organization. `rust-gpu` is still used by Embark and this repository still
gets used for work specific to us. However, its main home has relocated to the community.

#### [**Active development continues in its new home, under the Rust-GPU org!**](https://github.com/Rust-GPU/rust-gpu)

## How `rust-gpu` began

Historically in games GPU programming has been done through writing either HLSL, or to a lesser extent GLSL. These are simple programming languages that have evolved along with rendering APIs over the years. However, as game engines have evolved, these languages have failed to provide mechanisms for dealing with large codebases, and have generally stayed behind the curve compared to other programming languages.

In part this is because it's a niche language for a niche market, and in part this has been because the industry as a whole has sunk quite a lot of time and effort into the status quo. While over-all better alternatives to both languages exist, none of them are in a place to replace HLSL or GLSL. Either because they are vendor locked, or because they don't support the traditional graphics pipeline. Examples of this include CUDA and OpenCL. And while attempts have been made to create language in this space, none of them have gained any notable traction in the gamedev community.

Our hope with this project is that we push the industry forward by bringing an existing, low-level, safe, and high performance language to the GPU; namely [Rust](https://rust-lang.org). And with it come some additional benefits that can't be overlooked: a package/module system that's one of the industry's best, built in safety against race-conditions or out of bounds memory access, a wide range of tools and utilities to improve programmer workflows, and many others!

## Backwards compatibility, breaking changes and deprecation

Right now because the project is in an early state of development, we might introduce temporary changes as stop-gap measures, or implement features or APIs that might not work exactly in a way we end up liking. Therefore it is expected that some (if not most) of the user facing code will change and evolve over time. At the moment this means that we make no guarantees about backwards compatibility and have no formal deprecation model in place. Effectively meaning that currently we only support building from source with the latest `main` branch in our repository. We appreciate our early adopters and would ask them to evolve their code along with ours.

Please read our [Contributor Guide](CONTRIBUTING.md) for more information on how to get started.

## License

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.
