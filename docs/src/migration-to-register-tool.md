# Migration to `register_tool`

This document applies to [PR#926](https://github.com/EmbarkStudios/rust-gpu/pull/926)

## What happened

In a [recent nightly Rust update](https://github.com/rust-lang/rust/commit/76dd5c58a011bb734ad5b8e96fc560374893bc8f), the `register_attr` feature was removed in favor of `register_tool`. Unfortunately, rust-gpu made use of this feature to register the `spirv` attribute.

## What does this mean for you as a shader maintainer

You'll need to import the `spirv` proc macro attribute from `spirv-std` in order for the `spirv` macro to be visible in global scope:

```rust
use spirv_std::spirv;
```

If your shader code already contains this line but is conditionally only included for non-SPIR-V builds, like so:

```rust
#[cfg(not(target_arch = "spirv"))]
use spirv_std::spirv;
```

please remove the conditional attribute (the line containing `#[cfg(..)]`).

For this macro attribute to work correctly, it is important that `spirv` is visible in the global score and you use it like you used it before: `#[spirv(..)]`. An attempt to scope the attribute (such as `#[spirv_std::spirv(..)]`) will confuse the macro and it will likely fail to compile.

You'll also need to remove the `feature(register_attr)` and `register_attr(spirv)` attributes from your shader crates. If you're building using `SpirvBuilder`, you don't need to do anything else; the new `register_tool` is applied automatically. If not, you'll need to include these attributes instead:

```rust
#![feature(register_tool)]
#![register_tool(rust_gpu)]
```

That's it. Your shaders should now compile like before.

## Technical Background

Unfortunately, since the new Rust nightly toolchain in September 2022, `register_attr(spirv)` can no longer be used to register a global `spirv` attribute. Without this registration, the compiler would simply complain about `spirv` being an unknown attribute. However, the alternative, `register_tool`, requires us to scope the attribute in a namespace. For instance, as we've chosen the `rust_gpu` namespace, this would mean that you'd need to start writing `#[rust_gpu::spirv(..)]` instead, which would be quite tedious and would break a lot of code. And it's not possible to `use` a name from a tool namespace to bring it into scope.

Instead, we opted to implement a proc macro attribute called `spirv` instead[^1]. This macro attribute scans the item it is applied to, and translates any `#[spirv(..)]` it finds into `#[rust_gpu::spirv(..)]` which will be subsequently handled by the codegen backend. Because it is now a proc macro attribute exported from `spirv_std`, you need to do `use spirv_std::spirv` to make it globally visible in your crate. ***Note that we recommend using the `spirv` proc macro attribute itself rather than the `rust_gpu::spirv` attribute it translates to, as the latter is subject to change.***

We've also added the `feature(register_tool)` and `register_tool(rust_gpu)` crate attributes by default when compiling through `SpirvBuilder`. This will silence any error that you would otherwise get for applying a `rust_gpu` scoped attribute.

[^1]: This is not entirely true. In reality, the `spirv` proc macro attribute already existed, but only for non-spirv builds. It was used to turn the `#[spirv(..)]` attribute into a no-op. The proc macro is now used on all platforms, and it emits `#[cfg_attr(target_arch="spirv", rust_gpu::spirv(..))]` for each usage of `#[spirv(..)]`.
