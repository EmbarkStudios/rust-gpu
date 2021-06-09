# Image type syntax

There are a huge number of combinations of image types in SPIR-V. They are represented by a const
generic type called `spirv_std::image::Image`, however, specifying the generic parameters of this
type is incredibly tedious, so a wrapper macro, `spirv_std::Image!` can be used to write the type
instead.

The specific syntax and meaning of the arguments to the `Image!` macro can be found in
[rustdoc](https://embarkstudios.github.io/rust-gpu/api/spirv_std/macro.Image.html).

Some type aliases for common image formats can be found in the
[`spirv_std::image`](https://embarkstudios.github.io/rust-gpu/api/spirv_std/image/index.html)
module. For example, `Image2d` is a very commonly used type, corresponding to `texture2D` in GLSL,
and is likely what you want if you want a regular old sampled texture.

```rust,no_run
type Image2d = Image!(2D, type=f32, sampled);
```
