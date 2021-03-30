# Platform Support
The `rust-gpu` project currently supports a limited number of platforms and graphics APIs. Right now we're not distributing build artifacts and we're primarily focused on the development of the project, so this is based on the current `main` branch. There are a lot of different configurations and hardware out there to support, this document is intended to document what is currently supported, what we intend to support, and what isn't supported. Over time as the project stabilises and our CI improves, more platforms and APIs will be supported and tested. Currently support for each topic is divided into the following three categories.

- **Primary —** Built and tested on CI.
- **Secondary —** Built but *not fully tested* on CI.
- **Tertiary —** Present in the codebase but not built or tested.

## Operating System

| Operating System | Version | Support | Notes
|-------------------|---------|---------|-------
| Windows | 10+ | Primary | |
| Linux | Ubuntu 18.04+ | Primary | |
| macOS | Catalina (10.15)+ | Secondary | Using [MoltenVK], requires v1.1.2+
| Android | Tested 10-11 | Secondary | |

[MoltenVK]: https://github.com/KhronosGroup/MoltenVK

## Graphics APIs

| Name | Version | Support | Notes
|-------|---------|---------|-------
| SPIR-V | 1.3+ | Primary |
| Vulkan | 1.1+ | Primary |
| WGPU | 0.6 | Primary | Uses a translation layer to Metal/DX12
|  OpenGL | ??? | Tertiary |

### SPIR-V Targets

- `spirv-unknown-spv1.0`
- `spirv-unknown-spv1.1`
- `spirv-unknown-spv1.2`
- `spirv-unknown-spv1.3`
- `spirv-unknown-spv1.4`
- `spirv-unknown-spv1.5`

### Vulkan Targets

- `spirv-unknown-vulkan1.0`
- `spirv-unknown-vulkan1.1`
- `spirv-unknown-vulkan1.1spv1.4`
- `spirv-unknown-vulkan1.2`

### WebGPU Targets

- `spirv-unknown-webgpu0`

### OpenGL Targets

- `spirv-unknown-opengl4.0`
- `spirv-unknown-opengl4.1`
- `spirv-unknown-opengl4.2`
- `spirv-unknown-opengl4.3`
- `spirv-unknown-opengl4.5`

### OpenCL Targets

- `spirv-unknown-opencl1.2`
- `spirv-unknown-opencl1.2embedded`
- `spirv-unknown-opencl2.0`
- `spirv-unknown-opencl2.0embedded`
- `spirv-unknown-opencl2.1`
- `spirv-unknown-opencl2.1embedded`
- `spirv-unknown-opencl2.2`
- `spirv-unknown-opencl2.2embedded`

## GPU

Currently we don't have specific generations of GPUs for support, as long they support Vulkan 1.1+ with the latest officially installed drivers it should be able build and run the examples. You can check your Vulkan version using the [`vulkaninfo`] command from the `vulkan-sdk`.

##### Drivers
- [**AMD**][amd-drivers]
- **Intel:** [Linux][linux-intel], [macOS][macos-intel], [Windows][windows-intel]
- [**Nvidia**][nvidia-drivers]

[nvidia-drivers]: https://www.nvidia.com/Download/index.aspx?lang=en-us
[amd-drivers]: https://www.amd.com/en/support/kb/faq/gpu-56
[linux-intel]: https://www.intel.com/content/www/us/en/support/articles/000005520/graphics.html
[macOS-intel]: https://www.intel.com/content/www/us/en/support/articles/000022440/graphics.html
[windows-intel]: https://downloadcenter.intel.com/product/80939/Graphics
[`vulkaninfo`]: https://vulkan.lunarg.com/doc/view/latest/windows/vulkaninfo.html

