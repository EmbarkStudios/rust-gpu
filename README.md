# Rust on the GPU

This is a very early stage project to make Rust a first class langugage and ecosystem for building GPU code 🚀🚧

## Background

Historically in games GPU programming has been done through writing either HLSL, or to a lesser extent GLSL. These are simple programming languages that have evolved along with rendering APIs over the years. However, as game engines have evolved, these languages have failed to provide mechanisms for dealing with large codebases, and have generally stayed behind the curve compared to other programming languages.

In part this is because it's a niche language for a niche market, and in part this has been because the industry as a whole has sunk quite a lot of time and effort into the status quo. While over-all better alternatives to both languages exist, none of them are in a place to replace HLSL or GLSL. Either because they're vendor locked, or because they don't suppor the traditional graphics pipeline. Examples of this include CUDA and OpenCL. And while attempts have been made to create language in this space, none of them have gained any notable traction in the gamedev community.

Our hope with this project is that we push the industry forward by bringing an existing low-level, safe and high performance language to the GPU; that language being Rust. And with it come some additional benefits that can't be overlooked: a package/module system that's one of the industry's best, built in safety against race-conditions or out of bounds memory access etc.

## Why Embark?

At Embark, we've been building our in-house engine from the ground up in Rust and we have some previous in-house experience developing [RLSL](https://github.com/MaikKlein/rlsl) (a first prototype of this idea) and we have some of the world's best rendering engineers that are familiar with the problems in current shading languages so we're in a unique place to solve this problem. To streamline our own internal development, facilitate code-sharing between GPU and CPU, but most importantly: to enable our users to very rapidly build great looking experiences. If we do this project right one wouldn't nessisarily need a team of rendering engineers to build a good looking game, instead one would simply use a few of the existing open-source crates that provide the graphical effects needed to create the experience you're after. Instead of sharing snippets of TAA code on forum posts one could simply add the right crate(s).

## Project scope

This project will involve a few things if we want to get the experience right and it's quite broad. Initial stages will involve mostly just setting up the backend, however the project will be a bit broader then that.

- `rustc` compiler backend, either as a cranelift module, or as a seperate `rustc` backend (next to llvm and cranelift). We're currently evaluating both options, but it looks like a `rustc` native backend would be the most preferable.
- This compiler backend is currently planned to only support SPIR-V (the open compiler target for Vulkan) but it's not unlikely that in future versions this will / should support DXIL (the target for DirectX) or WHLSL (the WebGPU shading language that's bijective with SPIR-V)
- We'll need language front-end features to be able to support GPU workloads better; this will include among other things
  - Safe access to groupshared and constant memory
  - Support for intrinsics
  - Support for resource binding
- [crates.io](https://crates.io) support to be able to publish SPIR-V crates
- An Embark-provided rendering / framegraph abstraction to take advantage of this and to make it easy for users to re-use rendering effects.

The rustc compiler backend support levels are indicated by tiers; macOS, Linux and desktop PC are all Tier 1. However "smaller" platforms such as Android and iOS are Tier 2 and even smaller projects are Tier 3.

I think our initial goal should be to replace our own internal shaders with shaders written in Rust, and then to achieve Tier 3 support, which roughly means that it's possible to get up and running if you're willing to put in some effort into it. Usually this involves setting up a custom build environment, and getting it running in a custom environment. However, I think the ambition of the project ultimately should be Tier 1 support.
