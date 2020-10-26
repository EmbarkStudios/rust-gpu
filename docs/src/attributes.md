# Attribute syntax

rust-gpu introduces a number of SPIR-V related attributes to express behavior specific to SPIR-V not exposed in the base rust language.

There are a few different categories of attributes:

## Entry points

When declaring an entry point to your shader, SPIR-V needs to know what type of function it is. For example, it could be a fragment shader, or vertex shader. Specifying this attribute is also the way rust-gpu knows that you would like to export a function as an entry point, no other functions are exported.

Example:

```rust
#[spirv(fragment)]
fn main() { }
```

Common values are `#[spirv(fragment)]` and `#[spirv(vertex)]`. A list of all supported names can be found in [spirv_headers](https://docs.rs/spirv_headers/1.5.0/spirv_headers/enum.ExecutionModel.html) - convert the enum name to snake_case for the rust-gpu attribute name.

## Builtins

When declaring inputs and outputs, sometimes you want to declare it as a "builtin". This means many things, but one example is `gl_Position` from glsl - the GPU assigns inherent meaning to the variable and uses it for placing the vertex in clip space. The equivalent in rust-gpu is called `position`.

Example:

```rust:
#[spirv(fragment)]
fn main(
    #[spirv(position)] mut out_pos: Output<Vec4>,
) { }
```

Common values are `#[spirv(position)]`, `#[spirv(vertex_id)]`, and many more. A list of all supported names can be found in [spirv_headers](https://docs.rs/spirv_headers/1.5.0/spirv_headers/enum.BuiltIn.html) - convert the enum name to snake_case for the rust-gpu attribute name.

## Descriptor set and binding

A SPIR-V shader must declare where uniform variables are located with explicit indices that match up with CPU-side code. This can be done with the `descriptor_set` and `binding` attributes. Note that `descriptor_set = 0` is reserved for future use, and cannot be used.

Example:

```rust:
#[spirv(fragment)]
fn main(
    #[spirv(descriptor_set = 2, binding = 5)] mut var: Uniform<Vec4>,
) { }
```

Both descriptor_set and binding take an integer argument that specifies the uniform's index.
