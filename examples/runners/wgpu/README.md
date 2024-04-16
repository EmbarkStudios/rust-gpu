# WGPU Runner

This is a general runner for a fragment shader based on the [wgpu](https://wgpu.rs/) library
which is responsible for creating a target for rendering.

## `build.rs`

The `build.rs` file is responsible for running the builder, which will compile all the shaders
with targeting SPIR-V. This will use the SpirvBuilder from Rust-GPU to compile each of the
example shaders.

## Core functionality

The main file delegates immediately to `lib.rs` which will either call the compute shader or the
graphics shader depending on flags passed.

In the graphics file, the entry point is `start`, which first creates an event loop for handling
mouse clicks, and then creates a window using `winit`. It then performs set up for the window,
which allows for the window to immediately rendered to from the GPU.


