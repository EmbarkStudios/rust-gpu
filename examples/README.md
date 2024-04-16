# Examples

The examples here are split into a few categories:

- The shaders folder contain various rust-gpu shaders, and are examples of how to use rust-gpu.
  - The primary shader used as an example is the Sky Shader, which renders a single quad, and on
    this quad renders a sky.


- The runners folder contains programs that build and execute the shaders in the shaders folder using, for example,
  Vulkan. These programs are not exactly examples of how to use rust-gpu, as they're rather generic vulkan sample apps,
  but they do contain some infrastructure examples of how to integrate rust-gpu shaders into a build system (although
  both aren't the cleanest of examples, as they're also testing some of the more convoluted ways of consuming rust-gpu).
- Finally, the multibuilder folder is a very short sample app of how to use the `multimodule` feature of `spirv-builder`.

## Running an example

Each example can be run as follows:
```
# can substitute wgpu with cpu or ash
cd examples/runners/wgpu
cargo run --release
```

This should automatically build and compile rust-gpu, then compile the shader, then start the
shader.
