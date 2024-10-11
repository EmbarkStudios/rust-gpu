# What is a Shader?

"Shaders" are programs which historically rendered the "shades" (colors & light) of a 3D scene,
but now they generally refer to programs that run on the GPU. They can refer to general purpose
compute shaders, as an example for ML, or it can be a fixed function pipeline, such as
rasterizing a triangle, or running a function per mesh vertex.

# Rendering Shaders

Shaders were originally designed for rendering [triangle
meshes](https://en.wikipedia.org/wiki/Triangle_mesh) from the perspective of a camera.
There have traditionally between two distinct ways to render a mesh: rasterization and
raytracing, and we'll explain both.

Raytracing is a [physical
model](https://pbrt.org/) of light transport, and can be understood concisely as attempting to
model the real world. Some modern GPUs [support raytracing](https://developer.nvidia.com/rtx/raytracing/dxr/dx12-raytracing-tutorial-part-2), but as compared to rasterization
pipelines, they are more immature and have much less history (as of 2024). Raytracing is not
covered by this doc.

Rasterization is intended to emulate the appearance of a triangle, but more efficiently
than raytracing. Triangle vertices are represented as a 3D position in world space (with origin
as reference point), which are then translated into a position relative to a camera's view
(camera as reference point with rotated axes). For orthogonal or perspective cameras which are
the standard cameras used in rendering, the transformations from camera space to screen space
are defined by
[matrices](https://learnwebgl.brown37.net/08_projections/projections_perspective.html).
Generally, this step of projecting is done in the *vertex shader* which will transform a buffer
of vertices passed to the GPU and apply these transformations. Each vertex also has a relative
depth in screen space, so when triangles are rendered later, the closest triangles to the camera
can be rendered exclusively. The most common next stage is a *fragment shader*, which determines
what each part of visible triangles get rendered. An oversimplification but reasonable statement
is that a fragment shader takes the nearest visible triangle to the camera in that pixel and
determines a color for it. These shaders will write directly to an output buffer, such as a
window, or an output image.

# Compute Shaders

When someone talks about running ML code on the GPU, they are usually referring to compute
shaders in CUDA, which are general purpose programs, not specifically related to rendering.

Unlike rendering related shaders, compute shaders can be run on arbitrary data, but
compute shaders are generally run on 1D, 2D or 3D data. To handle this well, compute shaders are
invoked with some number of "local work groups". For example, it be (5, 5, 5) for handling a 5x5x5
matrix, or a (128, 128, 1) for handling a 128x128 image.

I/O to compute shaders must be done through explicit buffers or textures, as opposed to
rendering shaders which will have input from prior shaders.

As an example:
```
#[spirv(compute(threads(4,4,1)))]
pub fn handle_compute(
  #[spirv(global_invocation_id)] global_id: UVec3,
  #[spirv(local_invocation_id)] local__id: UVec3,
) {
  // step 1: use global_id and local_id to figure out which part of work needs to be done
  // step 2: do the work
  // step 3: ...
  // step 4: profit
}
```

## Usage Details

The way GLSL differentiate between shader kinds is usually that they are contained within fully
separate files and compiled separately. In Rust-GPU, they are marked with attributes:

```rust
#[spirv(fragment)]
fn fragment_shader(...) {

}

#[spirv(vertex)]
fn vertex_shader(...) {

}
```

The supported attributes are:`vertex, fragment, geometry, tesselation_control,
tessellation_evaluation`. The full list is in `crates/rustc_codegen_spirv/src/symbols.rs`.

# Why Shaders?

Shaders are generally preferred to code run on the CPU for programs where a lot of communication
need not be done between them. For rendering, each face can be projected in parallel, and then
the shade of each face can be computed separately. For compute shaders, it depends heavily on
the task, but for matrix multiplies and other tensor operations, there are efficient ways to
parallelize and implement them on GPU.

References:
- [Wikipedia](https://en.wikipedia.org/wiki/Shader#Ray_tracing_shaders)
- [The Book of (Fragment) Shaders](https://thebookofshaders.com/)
- [Compute Shaders](https://www.khronos.org/opengl/wiki/Compute_Shader)
