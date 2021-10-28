# Examples

The examples here are split into a few categories:

- The shaders folder contain various rust-gpu shaders, and are examples of how to use rust-gpu.
- The runners folder contains programs that build and execute the shaders in the shaders folder using, for example,
  Vulkan. These programs are not exactly examples of how to use rust-gpu, as they're rather generic vulkan sample apps,
  but they do contain some infrastructure examples of how to integrate rust-gpu shaders into a build system (although
  both aren't the cleanest of examples, as they're also testing some of the more convoluted ways of consuming rust-gpu).
- Finally, the multibuilder folder is a very short sample app of how to use the `multimodule` feature of `spirv-builder`.
