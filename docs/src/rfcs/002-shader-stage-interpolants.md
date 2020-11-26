# Summary

Shader stage interpolants are arguments used to communicate between shader stages. For simplicities sake we'll look at communicating between the Vertex Shader stage and Pixel Shader stage, and generalize later to other stages.

There is another concern here as well, which is vertex shader inputs from the input assembler. Typically these are the data that's in vertex buffers and index buffers, and it's either pre-fetched through special fixed function units or just accessed through global memory. On the API side of things this is part of the input assembler which is responsible for declaring vertex buffer layouts and optional format conversions. One thing to note here is that recently applications have started to bypass the input assembler in order to manually fetch vertex data that's needed in order to simplify application design.

# Explanation

Typically shaders communicate between stages using on-chip memory, so resources are usually limited or fall off of a performance cliff when using lots of them, so care must be taken into packing these resources together and wasting minimal space. Usually data here is packed together into 4 component vectors because hardware expects it. And a general rule of thumb would be that less parameters passed through, means more work can run in parallel (because the on-chip memory of fixed size can contain more vs/ps combintions).

## DirectX

DirectX mandates a minimum set of input and output slots available to the user of the API:

| Shader stage | Minimum requirement
| ---- | ---- |
| vertex input | 16 32-bit vectors (up to 4 components each)
| vertex output | 16 32-bit vectors (up to 4 components each)
| pixel input w/o geometry shader| 16 32-bit vectors (up to 4 components each)
| pixel input w/ geometry shader | 32 32-bit vectors (up to 4 components each)
| pixel output | 8 32-bit vectors (up to 4 components each) and 1 32-bit 1 component floating point depth value.

When the pipeline is configured without a geometry shader, a pixel shader is limited to 16, 32-bit, 4-component inputs (so 16 `int4`'s). Otherwise, a pixel shader can take up to 32, 32-bit, 4-component inputs (so 32 `int4`'s).

A pixel shader can output up to 8, 32-bit, 4-component colors, or no color if the pixel is discarded.  (so 8 `float4`'s).

### Linkage mechanism

In HLSL shader stages are linked by semanics, and can be incomplete (eg. one stage can export more then the next shader stage would import). While the semantics are documented and can be named; they don't actually imply anything other then which elements need to match up. Lots of semantics are available (`COLOR`, `BLENDWEIGHTS` etc) however, typically only `TEXCOORD[n]` is used to keep things positional and simple.

```hlsl
struct VertexInput
{
    float4 Position : POSITION;
    float3 Normal: NORMAL;
    float4 TexCoord: TEXCOORD0;
};

struct VertexOut
{
    float4 Position: SV_POSITION;
    float4 TexCoord: TEXCOORD0;
    float3 ViewNormal: NORMAL;
};

VertexOut VS(VertexInput input)
{
     // ...
}

struct PixelInput
{
    float4 Position: SV_POSITION;
    nointerpolation float4 TexCoord: TEXCOORD0;
};

float4 PS(PixelInput input)
{
    // ...
}
```


### Additional specifiers

On top of the usual syntax that hlsl has for specifying interpolants, there are some modifier keywords that express how the interpolation between stages is being done.

| Interpolation Modifier | Description
| ---- | ---- |
| linear | Interpolate between shader inputs; linear is the _default_ value if no interpolation modifier is specified.
| centroid | Interpolate between samples that are somewhere within the covered area of the pixel (this may require extrapolating end points from a pixel center). Centroid sampling may improve antialiasing if a pixel is partially covered (even if the pixel center is not covered). The centroid modifier must be combined with either the linear or noperspective modifier.
| nointerpolation |	Do not interpolate.
| noperspective | Do not perform perspective-correction during interpolation. The noperspective modifier can be combined with the centroid modifier.
| sample |  	Available in shader model 4.1 and laterInterpolate at sample location rather than at the pixel center.


## Vulkan Shader Stage limitations

In Vulkan no minima are enforced by the specification, however, an application can runtime query the amount of attributes that one can bind per shader stage. A quick survey on the Vulkan GPU Database gives:

| Shader Interface  | Locations Available | Min value | Max value
| ---- | ---- | ---- | ---- |
| vertex input | maxVertexInputAttributes | 16 | 4294969856
| vertex output  | maxVertexOutputComponents / 4 | 16 | 128
| tessellation control input | maxTessellationControlPerVertexInputComponents / 4 | 128 | 128
| tessellation control output | maxTessellationControlPerVertexOutputComponents / 4 | 128 | 128
| tessellation evaluation input | maxTessellationEvaluationInputComponents / 4 | 128 | 128
| tessellation evaluation output | maxTessellationEvaluationOutputComponents / 4 | 128 | 128
| geometry input | maxGeometryInputComponents / 4 | 64 | 128
| geometry output | maxGeometryOutputComponents / 4 | 128 | 128
| fragment input | maxFragmentInputComponents / 4 | 64 | 128
| fragment output | maxFragmentOutputAttachments | 4 | 8


### GLSL Linkage mechanism

Historically GLSL used to link shader stage interpolants by name, however in recent years it acquired the `layout(location = N, component = K)` attribute on interpolants. GLSL also allows partial linking where a previous stage can output more data then the stage before.

For pixel shader outputs in modern GLSL, instead of return a value from a function one writes to a global `out` type. And similarly, shader inputs also come from globals.

### Decorators

Similar to DirectX, Vulkan has the following decorations that can be applied to interpolants.

| Vulkan | DirectX equivalent
| ---- | ---- |
| - | linear |
| Flat| nointerpolation
| NoPerspective| noperspective
| Centroid | centroid
| Sample| sample

Vulkan has some additional explanations about these decorations as well:

> A variable decorated with `Flat` will not be interpolated. Instead, it will have the same value for every fragment within a triangle. This value will come from a single provoking vertex. A variable decorated with `Flat` can also be decorated with Centroid or Sample, which will mean the same thing as decorating it only as `Flat`.

## Metal shading language

__Mostly todo.__

One thing to note as a different to SPIR-V, GLSL and HLSL is that MSL doesn't seem to require explicit binding semantics or `Location` specifiers. It's specification has a `[[attribute(N)]]` specifier, but it's only used on tesselation control points and vertex shader inputs. It's specifically not used for communication between shader stages.

# SPIR-V Practicalities

Vertex shaders allow `Location` and `Component` decorations on input variable declarations. The `Location` decoration specifies which vertex input attribute is used to read and interpret the data that a variable will consume. The `Component` decoration allows the location to be more finely specified for scalars and vectors, down to the individual components within a location that are consumed. The components within a location are 0, 1, 2, and 3. A variable starting at component N will consume components N, N+1, N+2, …​ up through its size. For single precision types, it is invalid if the sequence of components gets larger than 3.


## Proposal

```rust
// these `attribute`'s would match the input assembler setup on the CPU
struct VertexInput {
    #[attribute(0)]
    position: Vec3,
    #[attribute(1)]
    normal: Vec3,
    #[attribute(2)]
    color: Vec3,
}

// naming this is hard, since vertex shader won't really know what shader
// stages will run after it until the pipeline state object gets created
// in vulkan. a tesselation shader may run after this for example,
// and that decision won't be known by our compiler.
struct VertexOutput {
    #[position]
    position: Vec3,
    normal: Vec3,
    color: Vec3,
}

fn vs_main(#[stage_in] input: &VertexInput) -> VertexOutput {
    VertexOutput {
        position: world_to_clip.mul(input.position),
        normal: input.normal,
        color: input.color,
    }
}

struct PixelOutput {
    // albedo and material id packed into same RGBA8 render target
    #[render_target(0)]
    albedo: Vec3,
    #[render_target(0)]
    material_idx: u8,

    // normal and exponent packed into RGBA8 render target
    #[render_target(1)]
    normal: Vec3,
    #[render_target(1)]
    specular_exponent: f32
}

fn ps_main(#[stage_in] input: &VertexOutput, shading: &ShadingInputs, indirect_lighting: &IndirectLighting) -> PixelOutput {
    let mut T = brdf(&shading);
    T += input.color;
    T += gi(&indirect_lighting);

    PixelOutput {
        albedo: T,
        packed_normal: input.normal,
        material_idx: 24,
        specular_exponent: ...
    }
}
```

 * Allowing mixed precision in the pixelshader outputs may be a mistake, historically it's been allowed to return a `float4` here and it would get packed into `RGBA8` for you. So it seems wise to keep that convention at least (and take advantage of pixel shader export hardware to do these conversions). However, adding `u8` in the mix here - while perfectly matching the expected output data format - might or might not be an issue. This should be discussed.