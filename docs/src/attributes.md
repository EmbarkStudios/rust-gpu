# Attribute syntax

rust-gpu introduces a number of SPIR-V related attributes to express behavior specific to SPIR-V not exposed in the base rust language.

Before you'll able to use these attributes, make sure you import the attribute from the `spirv-std` crate:

```rust
use spirv_std::spirv;
```

There are a few different categories of attributes:

## Entry points

When declaring an entry point to your shader, SPIR-V needs to know what type of function it is. For example, it could be a fragment shader, or vertex shader. Specifying this attribute is also the way rust-gpu knows that you would like to export a function as an entry point, no other functions are exported.

Example:

```rust
#[spirv(fragment)]
fn main() { }
```

Common values are `#[spirv(fragment)]` and `#[spirv(vertex)]`. A list of all supported names can be found in [spirv_headers](https://docs.rs/spirv_headers/1.5.0/spirv_headers/enum.ExecutionModel.html) - convert the enum name to snake_case for the rust-gpu attribute name.

### Compute shader dimensions

The dimensions (`local_size_*` in openGL, `numthreads` in DX) of a compute shader must be specified (eg. `#[spirv(compute(threads(32, 16, 97)))]`).  Trailing ones may be elided.

Example:

```rust
// the x dimension is required
// same as threads(32, 1, 1)
#[spirv(compute(threads(32)))]
pub fn compute_1() {}

// same as threads(32, 57, 1)
#[spirv(compute(threads(32, 57)))]
pub fn compute_2() {}
```

### Override entry point name

You can override the default `OpEntryPoint` name for any entry point with the `entry_point_name` sub-attribute on any of the execution model attributes. (e.g. `#[spirv(vertex(entry_point_name="foo"))]`)

## Builtins

When declaring inputs and outputs, sometimes you want to declare it as a "builtin". This means many things, but one example is `gl_Position` from glsl - the GPU assigns inherent meaning to the variable and uses it for placing the vertex in clip space. The equivalent in rust-gpu is called `position`.

Example:

```rust
#[spirv(vertex)]
fn main(
    #[spirv(position)] out_pos: &mut Vec4,
) { }
```

Common values are `#[spirv(position)]`, `#[spirv(vertex_id)]`, and many more. A list of all supported names can be found in [spirv_headers](https://docs.rs/spirv_headers/1.5.0/spirv_headers/enum.BuiltIn.html) - convert the enum name to snake_case for the rust-gpu attribute name.

## Descriptor set and binding

A SPIR-V shader must declare where uniform variables are located with explicit indices that match up with CPU-side code. This can be done with the `descriptor_set` and `binding` attributes. Note that `descriptor_set = 0` is reserved for future use, and cannot be used.

Example:

```rust
#[spirv(fragment)]
fn main(
    #[spirv(uniform, descriptor_set = 2, binding = 5)] var: &mut Vec4,
) { }
```

Both descriptor_set and binding take an integer argument that specifies the uniform's index.

## Flat

The flat attribute corresponds to the flat keyword in glsl - in other words, the data is not interpolated across the triangle when invoking the fragment shader.

Example:

```rust
#[spirv(fragment)]
fn main(#[spirv(flat)] obj: u32) { }
```

## Invariant

The invariant attribute corresponds to the invariant keyword in glsl. It can only be applied to output variables.

Example:

```rust
#[spirv(vertex)]
fn main(#[spirv(invariant)] var: &mut f32) { }
```

## Workgroup shared memory

The `workgroup` attribute defines shared memory, which can be accessed by all invocations within the same workgroup. This corresponds to `groupshared` memory in hlsl or `shared` memory in glsl.

Example:

```rust
#[spirv(compute(threads(32)))]
fn main(#[spirv(workgroup)] var: &mut [Vec4; 4]) { }
```

## Generic storage classes

The SPIR-V storage class of types is inferred for function signatures. The inference logic can be guided by attributes on the interface specification in the entry points. This also means it needs to be clear from the documentation if an API requires a certain storage class (e.g `workgroup`) for a variable. Storage class attributes are only permitted on entry points.

## Specialization constants

Entry point inputs also allow access to [SPIR-V "specialization constants"](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#SpecializationSection),
which are each associated with an user-specified numeric "ID" (SPIR-V `SpecId`),
used to override them later ("specializing" the shader):
* in Vulkan: [during pipeline creation, via `VkSpecializationInfo`](https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap10.html#pipelines-specialization-constants)
* in WebGPU: [during pipeline creation, via `GPUProgrammableStage`<i>`#constants`</i>](https://www.w3.org/TR/webgpu/#gpuprogrammablestage)
  * note: WebGPU calls them ["pipeline-overridable constants"](https://gpuweb.github.io/gpuweb/wgsl/#pipeline-overridable)
* in OpenCL: [via `clSetProgramSpecializationConstant()` calls, before `clBuildProgram()`](https://registry.khronos.org/OpenCL/sdk/3.0/docs/man/html/clSetProgramSpecializationConstant.html)

If a "specialization constant" is not overriden, it falls back to its *default*
value, which is either user-specified (via `default = ...`), or `0` otherwise.

While only "specialization constants" of type `u32` are currently supported, it's
always possible to *manually* create values of other types, from one or more `u32`s.

Example:

```rust
#[spirv(vertex)]
fn main(
    // Default is implicitly `0`, if not specified.
    #[spirv(spec_constant(id = 1))] no_default: u32,

    // IDs don't need to be sequential or obey any order.
    #[spirv(spec_constant(id = 9000, default = 123))] default_123: u32,

    // Assembling a larger value out of multiple `u32` is also possible.
    #[spirv(spec_constant(id = 100))] x_u64_lo: u32,
    #[spirv(spec_constant(id = 101))] x_u64_hi: u32,
) {
    let x_u64 = ((x_u64_hi as u64) << 32) | (x_u64_lo as u64);
}
```

<sub>**Note**: despite the name "constants", they are *runtime values* from the
perspective of compiled Rust code (or at most similar to "link-time constants"),
and as such have no connection to *Rust constants*, especially not Rust type-level
constants and `const` generics - while specializing some e.g. `fn foo<const N: u32>`
by `N` long after it was compiled to SPIR-V, or using "specialization constants"
as Rust array lengths, Rust would sadly require *dependent types* to type-check
such code (as it would for e.g. expressing C `T[n]` types with runtime `n`),
and the main benefit over truly dynamic inputs is a (potential) performance boost.<sub>
