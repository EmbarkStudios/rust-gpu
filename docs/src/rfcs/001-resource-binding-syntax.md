
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

There are some constraints that currently exist when it comes to binding resources to shaders that we may need to take into account, Vulkan has `maxBoundDescriptorSets` for example (between 4 and 32). However for the sake of argument we'll ignore these practical limitations for now.
</details>

# Goal 
Ideally what one would do is have a group of bindings collected together logically for each rendering system; where the bindings are set up (semi-)automatically on both the GPU and CPU.

One thing we'd like to avoid is having a set of names or numbers that manually need to match up between GPU and CPU. 

1. Manual numbers (or strings) aren't type-safe.
1. They make refactoring more difficult (need to change both CPU and GPU side).
1. They can clash, especially in large code bases where it's difficult to keep track.

# Overview

I would like us to have an easy way to connect bindings on the GPU and CPU side together, in a way that's as ergonomic to use as possible.

## Globals

<details>

```rust
#![feature(const_generics)]
#![feature(const_fn)]
use core::marker::PhantomData;

struct Texture<T: Default, const SET: u32, const SLOT: u32> {
    d: PhantomData<T>
}

impl<T: Default, const SET: u32, const SLOT: u32> Texture<T, SET, SLOT> {
    const fn new() -> Self {
        Self {
            d: PhantomData
        }
    }
    
    fn sample(&self, _u: f32, _v: f32) -> T {
        T::default()
    }
}
```
</details>


This is a more traditional example that's closer to how HLSL or GLSL would do bindings, they have a few generic parameters and it relies on `const_generic` to function. If we mirror this in existing Rust syntax we'll get something like this:

```rust
static ALBEDO : Texture::<f32, 0, 0> = Texture::new();
static NORMAL_MAP : Texture::<f32, 0, 1> = Texture::new();
static SMOOTHNESS : Texture::<f32, 0, 2> = Texture::new();
static LIGHTMAP : Texture::<f32, 0, 3> = Texture::new();

fn main() {
    // functions can access the globals directly
    let mut T = brdf();
    T += gi();
}
```

This seems to have a few downsides:

 - All caps for the variable/buffer name.
 - Right now a constructor call is required, and it should be const.
 - Need to ensure no overlap between sets & slots.
 - In big code-bases, the dependencies on shaders become very blurred and one needs to inveriable figure out which bindings don't get DCE'd, and more importantly which bindings are actually being used.
 - If you're using a crate authored by a 3rd-party, it quickly becomes non-obvious which resources need to be bound.
 - Unclear which downstream call-sites end up requiring more bindings to your SPIR-V blob.

And a few upsides:

 - Globals can be accessed anywhere in the program, easily.

Alternative would be to remove the `const_generic` parameters potentially by moving them into the constructor.

```rust
static ALBEDO : Texture::<f32> = Texture::new(0, 0);
static NORMAL_MAP : Texture::<f32> = Texture::new(0, 1);
static SMOOTHNESS : Texture::<f32> = Texture::new(0, 2);
static LIGHTMAP : Texture::<f32> = Texture::new(0, 3);
```

This would make it easier to pass textures to functions (even if they're in different slots) because you'd get something like this:

```rust
fn some_system(tex: &Texture::<f32>) {
    let v = tex.sample(0.0, 0.0);
}
```

Instead of this, which would tightly couple the whole downstream system to have a texture bound to a specific set and slot. This would make changing the bindings later on a nightmare, and wouldn't allow you to conditionally invoke `some_system` with textures bound to different locations.

```rust
fn some_system(tex: &Texture::<f32, 0, 0>) {
    let v = tex.sample(0.0, 0.0);
}
```

## Arguments to `main`

Most compute-only languages tend to prefer this along with positional binding. This makes "invoking a shader" look much more like a function call and uses existing and familiar semantics.

```rust
fn main(albedo: Texture::<f32>, normal_map: Texture::<f32>, smoothness: Texture::<f32>, lightmap: Texture<f32>) {
    let mut T = brdf(&albedo, &normal_map, &smoothness);
    T += gi(&lightmap);
}
```

Doing this straight up has a few downsides:

 - Big kernels/shader can have lots of buffers and textures bound making this list potentially large.
 - Need to pass down all bindings all the way to leaf-node systems.
 
Some upsides:

 - No clashes for sets/slots since the language takes care of this.
 - It's clear from the get-go which bindings are being used.

A much nicer and more ergonomic approach would be to store texture bindings in structs:

```rust
struct ShadingInputs {
    albedo: Texture::<f32>,
    normal_map: Texture::<f32>,
    smoothness: Texture::<f32>,
}

struct IndirectLighting {
    lightmap: Texture<f32>,
}

fn main(inputs: &ShadingInputs, indirect_lighting: &IndirectLighting) {
    let mut T = brdf(&inputs);
    T += gi(&indirect_lighting);
}
```

# Suggestion

I think the most ergonomic and future proof binding method would be to have descriptors in structs, bound to the entrypoint. This allows us some nice, even more ergonomic upsides later on (when support is more widely available) where we can put data members in these structs as well. And along with this, we can have very egonomic CPU side code as well, where we can keep shader invocation looking like a function call for a large part, instead of having to manually bind to slots again.

# Prior art

<details>

## Metal

Metal has argument buffers, best described here: https://developer.apple.com/documentation/metal/buffers/about_argument_buffers

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
</details>
