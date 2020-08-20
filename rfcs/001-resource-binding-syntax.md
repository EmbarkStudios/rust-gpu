
# Summary

We aught to discuss how we want resource binding to work on a language syntax level.

# Explanation

<details>
<summary>Small overview of descriptors</summary>
Historically resources such as buffers, textures and samplers have been special in hardware where they were bound to specific slots on the shader core. Usually a limited number of such slots existed, leading to platform and vendor specific limits.

### DirectX 
In recent years most Desktop platforms have switched to loading resource descriptors from GPU memory instead, this has the major advantage that there is no real limit on the amount of Textures, Buffers and other resource anymore because they all can come from memory. In DirectX12 hardware is divided up into several Tiers which each denominate the amount of descriptors available.

| Resources Available to the Pipeline| 	Tier 1	| Tier 2	| Tier 3|
| ---- | ---- | ---- | ---- |
| Feature levels| 	11.0+	| 11.0+| 	11.1+|
| Maximum number of descriptors in a Constant Buffer View (CBV), Shader Resource View (SRV), or Unordered  Access View(UAV) heap used for rendering|	1,000,000	|1,000,000	|1,000,000+|
| Maximum number of Constant Buffer Views in all descriptor tables per shader stage	|14	|14	|full heap
| Maximum number of Shader Resource Views in all descriptor tables per shader stage	|128	|full heap	|full heap
| Maximum number of Unordered Access Views in all descriptor tables across all stages	|64 for feature levels 11.1+ or 8 for feature level | 64|full heap
| Maximum number of Samplers in all descriptor tables per shader stage|	16|	full heap|	full heap

### Vulkan
On the other hand in Vulkan resource descriptor binding is designed in such a way that it can accomodate hardware that still relies on descriptor slots instead of memory, because still a lot of such hardware exists in the wild. Especially in the Mobile space.
</details>

# Suggestion

The simple initial suggestion would be to only support positional binding to shaders at the moment. This has some drawbacks (such as requiring a new root signature / vulkan shader pipeline layout for every shader) and so it doesn't allow you to override the descriptor set index or binding slot so you end up giving up some flexibility.

```rust
fn compute(buffer: Buffer<RuntimeArray<f32>>)
```

Ideally these bindings can also appear in structs that go into the shader/kernel so that each system can have a struct with their input parameters. This would most likely be modeled similar to Metal's Argument Buffers so that one can do things like this:

```rust
struct LightingArgs {
    shadow_map: Texture<f32>,
    material_info: Buffer<RuntimeArray<MaterialInfo>>
}

fn compute(lighting_info: LightingArgs)
```


# Drawbacks

List potential issues that one would want to have discussed in the RFC comment section

# Alternatives



# Prior art

## Metal

## HLSL
Resource binding in HLSL is done by declaring a set of special case globals that make up resource descriptors on the GPU. 

```hlsl
Texture2D<float4> tex0          : register(t5,  space0);
Texture2D<float4> tex1[][5][3]  : register(t10, space0);
Texture2D<float4> tex2[8]       : register(t0,  space1);
SamplerState samp0              : register(s5, space0);
ConstantBuffer<myConstants>   c[10000] : register(b0);
```

In DirectX12 the feature of Root Signatures got added that is essentially a new domain specific language to set up the layout / calling convention of the shader; specified in a string.

Note the the two examples don't match necessarily. 

```hlsl
#define MyRS1 "RootFlags( ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT | " \
                         "DENY_VERTEX_SHADER_ROOT_ACCESS), " \
              "CBV(b0, space = 1, flags = DATA_STATIC), " \
              "SRV(t0), " \
              "UAV(u0), " \
              "DescriptorTable( CBV(b1), " \
                               "SRV(t1, numDescriptors = 8, " \
                               "        flags = DESCRIPTORS_VOLATILE), " \
                               "UAV(u1, numDescriptors = unbounded, " \
                               "        flags = DESCRIPTORS_VOLATILE)), " \
              "DescriptorTable(Sampler(s0, space=1, numDescriptors = 4)), " \
              "RootConstants(num32BitConstants=3, b10), " \
              "StaticSampler(s1)," \
              "StaticSampler(s2, " \
                             "addressU = TEXTURE_ADDRESS_CLAMP, " \
                             "filter = FILTER_MIN_MAG_MIP_LINEAR )"
```

## GLSL

```glsl
uniform texture2D inputTex;
uniform restrict writeonly uimage2D outputTex;

layout(std430) buffer mesh_color_buf {
    vec4 colors[];
};

layout(std430) buffer mesh_vertex_buf {
    VertexPacked vertices[];
};
```

## CUDA

In CUDA most resources are bound to the kernel's entry point - e.g. buffers are just passed as pointers etc. 

## OpenCL

In OpenCL resources are marked up with their address spaces and also passed as function arguments to the executing kernel. Resources like buffers look like pointers but a lot of compiler magic is going on to turn them from resource descriptors (such as the GCN V#) into something that can emulate pointers.

## RLSL

RLSL also passes resource bindings as arguments to the kernel, with two template parameters. One for the `set` and one for the `space`.

```rust
#[spirv(compute)]
fn compute(compute: Compute, buffer: Buffer<N0, N0, RuntimeArray<f32>>)
```