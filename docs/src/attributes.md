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
