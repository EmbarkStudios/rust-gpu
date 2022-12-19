# `spirv-std`

Core functions, traits, and more that make up a “standard library” for SPIR-V for use in [rust-gpu](https://github.com/EmbarkStudios/rust-gpu#readme).

This crate gives a `rust-gpu` shader access to the required `#![spirv(..)]` attribute, as well as povide all kinds of APIs that allows a shader to access GPU resources such as textures and buffers. Optionally, through the use of the `"glam"` feature, it includes some boilerplate trait implementations to make `glam` vector types compatible with these APIs.

## 🚨 BREAKING 🚨

As of `0.4.0-alpha.16`, your shaders will require a different preamble. See [this doc][migration] for more information.

## Example

![Sky shader](https://github.com/EmbarkStudios/rust-gpu/raw/main/docs/assets/sky.jpg)

```rust
use spirv_std::spirv;
use glam::{Vec3, Vec4, vec2, vec3};

#[spirv(fragment)]
pub fn main(
    #[spirv(frag_coord)] in_frag_coord: &Vec4,
    #[spirv(push_constant)] constants: &ShaderConstants,
    output: &mut Vec4,
) {
    let frag_coord = vec2(in_frag_coord.x, in_frag_coord.y);
    let mut uv = (frag_coord - 0.5 * vec2(constants.width as f32, constants.height as f32))
        / constants.height as f32;
    uv.y = -uv.y;

    let eye_pos = vec3(0.0, 0.0997, 0.2);
    let sun_pos = vec3(0.0, 75.0, -1000.0);
    let dir = get_ray_dir(uv, eye_pos, sun_pos);

    // evaluate Preetham sky model
    let color = sky(dir, sun_pos);

    *output = tonemap(color).extend(1.0)
}
```

See [source][source] for full details.

## Getting started

Check out [The `rust-gpu` Dev Guide][gpu-guide] for information on how to get started with using it in your projects.

Experiment with rust-gpu shaders in-browser at [SHADERed][shadered].

[migration]: https://github.com/EmbarkStudios/rust-gpu/blob/main/docs/src/migration-to-register-tool.md
[source]: https://github.com/EmbarkStudios/rust-gpu/blob/main/examples/shaders/sky-shader/src/lib.rs
[gpu-guide]: https://embarkstudios.github.io/rust-gpu/book/
[shadered]: https://shadered.org/shaders?language=rust&sort=hot
