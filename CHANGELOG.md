# `rust-gpu` Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- NOTE(eddyb) sections from the original template:

### Added ⭐
- New features go here in a bullet list

### Changed 🛠
- Changes to existing functionality go here in a bullet list

### Deprecated 🚧
- Mark features soon-to-be removed in a bullet list

### Removed 🔥
- Features that have been removed in a bullet list

### Fixed 🩹
- Bug fixes in a bullet list

### Security 🔐
- Changes/fixes related to security vulnerabilities in a bullet list

-->

## [Unreleased]

### Added ⭐
- [PR#959](https://github.com/EmbarkStudios/rust-gpu/pull/959) added two `spirv-builder` environment variables to customize *only* the `rustc` invocations for shader crates and their dependencies:
  - `RUSTGPU_RUSTFLAGS="..."` for shader `RUSTFLAGS="..."`
  - `RUSTGPU_CODEGEN_ARGS="..."` for shader "codegen args" (i.e. `RUSTFLAGS=-Cllvm-args="..."`)  
    (check out [the "codegen args" docs](docs/src/codegen-args.md), or run with `RUSTGPU_CODEGEN_ARGS=--help` to see the full list of options)
- [PR#940](https://github.com/EmbarkStudios/rust-gpu/pull/940) integrated the experimental [`SPIR-🇹` shader IR framework](https://github.com/EmbarkStudios/spirt) into the linker  
  (opt-in via `RUSTGPU_CODEGEN_ARGS=--spirt`, see also [the `--spirt` docs](docs/src/codegen-args.md#--spirt), for more details)

### Changed 🛠️
- [PR#958](https://github.com/EmbarkStudios/rust-gpu/pull/958) updated toolchain to `nightly-2022-10-29`
- [PR#941](https://github.com/EmbarkStudios/rust-gpu/pull/941) applied workspace inheritance to `Cargo.toml` files
- [PR#959](https://github.com/EmbarkStudios/rust-gpu/pull/959) moved `rustc_codegen_spirv` debugging functionality from environment variables to "codegen args" options/flags (see [the updated docs](docs/src/codegen-args.md) for more details)
- [PR#967](https://github.com/EmbarkStudios/rust-gpu/pull/967) made `--dump-*` ["codegen args"](docs/src/codegen-args.md) include identifying information (e.g. crate names) in the names of files they emit

### Removed 🔥
- [PR#946](https://github.com/EmbarkStudios/rust-gpu/pull/946) removed the `fn`/closure `#[spirv(unroll_loops)]` attribute, as it has no users, is becoming non-trivial to support, and requires redesign for better ergonomics (e.g. `#[spirv(unroll)]` applied to individual loops, not the whole `fn`/closure)

## [0.4.0-alpha.17]

### Fixed 🩹
- [PR#937](https://github.com/EmbarkStudios/rust-gpu/pull/937) fixed Rust-GPU crates not referring to each-other by exact version
- [PR#937](https://github.com/EmbarkStudios/rust-gpu/pull/937) fixed `spirv-std` referring to an older version of `spirv-std-macros`

## [0.4.0-alpha.16]

### Added ⭐
- [PR#935](https://github.com/EmbarkStudios/rust-gpu/pull/935) added check for environment variable `RUSTGPU_SKIP_TOOLCHAIN_CHECK` to prevent toolchain check

### Changed 🛠️
- 🚨BREAKING🚨 [#926](https://github.com/EmbarkStudios/rust-gpu/pull/926) migrated from `register_attr` to `register_tool`. [More information](docs/src/migration-to-register-tool.md).
- [PR#935](https://github.com/EmbarkStudios/rust-gpu/pull/935) updated toolchain to `nightly-2022-10-01`
- [PR#934](https://github.com/EmbarkStudios/rust-gpu/pull/934) updated `glam` to `0.22`
- [PR#928](https://github.com/EmbarkStudios/rust-gpu/pull/928) updated `spirv-tools` to `0.9` (SPIRV-Tools `2022.4`)

### Removed 🔥
- [PR#934](https://github.com/EmbarkStudios/rust-gpu/pull/934) Removed `glam::BVec` support (they are no longer `#[repl(simd)]` in `glam`, as Rust doesn't support SIMD vectors with `bool` elements)

### Fixed 🩹
- [PR#927](https://github.com/EmbarkStudios/rust-gpu/pull/927) re-taught Cargo to rebuild shader crates when `rustc_codegen_spirv` is rebuilt, via [`-Zbinary-dep-depinfo`](https://github.com/rust-lang/rust/pull/93969) (broken since a toolchain update in `0.4.0-alpha.13`, and has been causing spurious build failures ever since)

## [0.4.0-alpha.15]

### Added ⭐
- [PR#919](https://github.com/EmbarkStudios/rust-gpu/pull/919) added a build-time check for the nightly toolchain version, to provide user-friendly error messages

### Changed 🛠️
- [PR#918](https://github.com/EmbarkStudios/rust-gpu/pull/918) updated toolchain to `nightly-2022-08-29`

## [0.4.0-alpha.14]

### Changed 🛠
- [PR#904](https://github.com/EmbarkStudios/rust-gpu/pull/904) renamed helper `spirv-types` crate to `spirv-std-types`

## [0.4.0-alpha.13]

### Added ⭐
- [PR#717](https://github.com/EmbarkStudios/rust-gpu/pull/717) added `noreturn` support to inline `asm!`
- [PR#742](https://github.com/EmbarkStudios/rust-gpu/pull/742) added a `spirv-builder` option to include all debug info
- [PR#787](https://github.com/EmbarkStudios/rust-gpu/pull/787) documented `Cargo.toml` `[profile.⋯.build-override]` setting for avoiding slow builds
- [PR#830](https://github.com/EmbarkStudios/rust-gpu/pull/830) added a `spirv-builder` option to preserve unused descriptor bindings
- [PR#875](https://github.com/EmbarkStudios/rust-gpu/pull/875) added warnings for `#[inline(never)]`s the inliner doesn't respect (for legalization reasons)
- [PR#848](https://github.com/EmbarkStudios/rust-gpu/pull/848) added `#[spirv(subgroup_local_invocation_id)]` built-in (and an `examples/shaders/reduce` compute shader)
- new `spirv-std` APIs: `ByteAddressableBuffer`<sup>[#735](https://github.com/EmbarkStudios/rust-gpu/pull/735)</sup>, `SampledImage::sample_by_lod`<sup>[#755](https://github.com/EmbarkStudios/rust-gpu/pull/755)</sup>, `arch::read_clock_khr`<sup>[#757](https://github.com/EmbarkStudios/rust-gpu/pull/757)</sup>, `arch::{signed,unsigned}_{min,max}`<sup>[#763](https://github.com/EmbarkStudios/rust-gpu/pull/763)</sup>, `debug_printf!`<sup>[#768](https://github.com/EmbarkStudios/rust-gpu/pull/768)</sup>, `arch::*memory_barrier*`<sup>[#769](https://github.com/EmbarkStudios/rust-gpu/pull/769)</sup>, `arch::IndexUnchecked`<sup>[#805](https://github.com/EmbarkStudios/rust-gpu/pull/805)</sup>, `RayQuery::confirm_intersection`<sup>[#822](https://github.com/EmbarkStudios/rust-gpu/pull/822)</sup>, `arch::atomic_i_increment`<sup>[#839](https://github.com/EmbarkStudios/rust-gpu/pull/839)</sup>, `arch::atomic`<sup>[#877](https://github.com/EmbarkStudios/rust-gpu/pull/877)</sup>

### Changed 🛠
- [PR#743](https://github.com/EmbarkStudios/rust-gpu/pull/743) set the SPIR-V "generator magic number" to [the value reserved for Rust-GPU](https://github.com/KhronosGroup/SPIRV-Headers/pull/174)
- [PR#761](https://github.com/EmbarkStudios/rust-gpu/pull/761) made `spirv-std` build on stable Rust by avoiding `enum`s in `const`-generics
- [PR#784](https://github.com/EmbarkStudios/rust-gpu/pull/784) documented `spirv-std` throughout
- [PR#862](https://github.com/EmbarkStudios/rust-gpu/pull/862) updated toolchain to `nightly-2022-04-11`
- [PR#846](https://github.com/EmbarkStudios/rust-gpu/pull/846) updated `spirv-tools` to `0.8` (SPIRV-Tools `2022.1`)
- [PR#888](https://github.com/EmbarkStudios/rust-gpu/pull/888) widened the supported `glam` version range to `0.17`-`0.21`

### Fixed 🩹
- [PR#729](https://github.com/EmbarkStudios/rust-gpu/pull/729) fixed [#723](https://github.com/EmbarkStudios/rust-gpu/issues/723) by explicitly allowing unused shader inputs/outputs in storage class inference
- [PR#732](https://github.com/EmbarkStudios/rust-gpu/pull/732) fixed `rustc` ICE messages being truncated with `rustc_codegen_spirv` (broken since a toolchain update in `0.4.0-alpha.12`)
- [PR#737](https://github.com/EmbarkStudios/rust-gpu/pull/737) fixed [#642](https://github.com/EmbarkStudios/rust-gpu/issues/642) by re-adding `-Zsymbol-mangling-version=v0` (for generic parameters in `fn` names)
- [PR#766](https://github.com/EmbarkStudios/rust-gpu/pull/766) fixed [#373](https://github.com/EmbarkStudios/rust-gpu/issues/373) and [#731](https://github.com/EmbarkStudios/rust-gpu/issues/731) by hooking `rustc` to limit function call ABIs to what Rust-GPU supports
- [PR#793](https://github.com/EmbarkStudios/rust-gpu/pull/793) fixed [#748](https://github.com/EmbarkStudios/rust-gpu/issues/748) by supporting SPIR-V `1.4`'s changed entry-point rules
- [PR#844](https://github.com/EmbarkStudios/rust-gpu/pull/844) fixed [#836](https://github.com/EmbarkStudios/rust-gpu/issues/836) by making newtypes (e.g. single-field structs) wrapping `ScalarPair`s (e.g. `(u32, u32)` or `&[T]`) have the same SPIR-V type as their inner field

## [0.4.0-alpha.12]

### Added ⭐
- [PR#704](https://github.com/EmbarkStudios/rust-gpu/pull/704) added `Image::gather` and `Image::sample_bias` to `spirv-std`
- [PR#709](https://github.com/EmbarkStudios/rust-gpu/pull/709) added float packing/unpacking operations to `spirv-std`

### Changed 🛠
- [PR#716](https://github.com/EmbarkStudios/rust-gpu/pull/716) updated toolchain to `nightly-2021-08-10`

### Removed 🔥
- [PR#710](https://github.com/EmbarkStudios/rust-gpu/pull/710) removed "implicit bindless" and kernel modes

## [0.4.0-alpha.11]

### Changed 🛠
- [PR#702](https://github.com/EmbarkStudios/rust-gpu/pull/702) updated `glam` to `0.17`

## [0.4.0-alpha.10]

### Added ⭐
- [PR#655](https://github.com/EmbarkStudios/rust-gpu/pull/655) added a `watch` feature to `spirv-builder` for hot reloading shaders
- [PR#652](https://github.com/EmbarkStudios/rust-gpu/pull/652) documented `Image!` in the Rust-GPU book
- [PR#660](https://github.com/EmbarkStudios/rust-gpu/pull/660) added a `spirv-builder` option to name global `OpVariables`
- [PR#662](https://github.com/EmbarkStudios/rust-gpu/pull/662) added type aliases for common uses of `Image!`
- [PR#683](https://github.com/EmbarkStudios/rust-gpu/pull/683) added a `spirv-builder` option to treat warnings as errors

### Changed 🛠
- [PR#672](https://github.com/EmbarkStudios/rust-gpu/pull/672) updated toolchain to `nightly-2021-06-09`
- [PR#674](https://github.com/EmbarkStudios/rust-gpu/pull/674) updated `glam` to `0.16`

### Removed 🔥
- [PR#666](https://github.com/EmbarkStudios/rust-gpu/pull/666) removed `arch::arithmetic` from `spirv-std`

## [0.4.0-alpha.9]

### Fixed 🩹
- fixed miscompilation in peephole optimizations (see [PR#646](https://github.com/EmbarkStudios/rust-gpu/pull/646))

## [0.4.0-alpha.8]

### Added ⭐
- [PR#608](https://github.com/EmbarkStudios/rust-gpu/pull/608) added `Image::query_*` operations to `spirv-std`
- [PR#610](https://github.com/EmbarkStudios/rust-gpu/pull/610) added `spirv-builder` support for enabling extra extensions and/or capabilities
- [PR#612](https://github.com/EmbarkStudios/rust-gpu/pull/612) added `is_helper_invocation` to `spirv-std`
- [PR#624](https://github.com/EmbarkStudios/rust-gpu/pull/624) added `OpTypeSampler` and `OpTypeAccelerationStructureKHR` support to inline `asm!`
- [PR#622](https://github.com/EmbarkStudios/rust-gpu/pull/622) added the ability to query entry-point names from `spirv-builder`
- [PR#630](https://github.com/EmbarkStudios/rust-gpu/pull/630) added a more convenient API to `spirv-builder`, for requesting extensions and/or capabilties
- [PR#629](https://github.com/EmbarkStudios/rust-gpu/pull/629) added an optimization to convert N identical scalar (e.g. arithmetic) ops, into one N-wide vector op (as e.g. `glam` only emits the former)
- [PR#596](https://github.com/EmbarkStudios/rust-gpu/pull/596) added a `RuntimeArray` type to `spirv-val`, to represent SPIR-V `OpRuntimeArray`s
- [PR#635](https://github.com/EmbarkStudios/rust-gpu/pull/635) added several `spirv-builder` options for controlling `spirv-val` flags
- [PR#643](https://github.com/EmbarkStudios/rust-gpu/pull/643) added `Image::read_subpass` to `spirv-std`

### Changed 🛠
- [PR#616](https://github.com/EmbarkStudios/rust-gpu/pull/616) updated `spirv-tools` to `0.6.1` and turned on emission of line-based debug info
- [PR#631](https://github.com/EmbarkStudios/rust-gpu/pull/631) updated toolchain to `nightly-2021-05-24`
- [PR#641](https://github.com/EmbarkStudios/rust-gpu/pull/641) made `spirv-std` depend on `glam` (`0.15.2`), instead of the other way around

## [0.4.0-alpha.7]

### Fixed 🩹
- [PR#607](https://github.com/EmbarkStudios/rust-gpu/pull/607) removed accidental use of `feature(or_patterns)` (recently stabilized, only on nightly)

## [0.4.0-alpha.6]

### Added ⭐
- [PR#586](https://github.com/EmbarkStudios/rust-gpu/pull/586) added support for constant memory (`&'static _` references), within the limits of SPIR-V
- [PR#559](https://github.com/EmbarkStudios/rust-gpu/pull/559) added the ability to set a Rust "target triple" in `spirv-builder` (e.g. `"spirv-unknown-vulkan1.1"` for Vulkan `1.1`)
- [PR#563](https://github.com/EmbarkStudios/rust-gpu/pull/563) added `SPV_KHR_ray_tracing` APIs to `spirv-std`
- [PR#572](https://github.com/EmbarkStudios/rust-gpu/pull/572) added `SPV_KHR_ray_query` APIs to `spirv-std`
- [PR#359](https://github.com/EmbarkStudios/rust-gpu/pull/359) added a `const`-generic `Image` type, and `Image!` macro wrapping it (to add "named parameters"), to `spirv-std`

### Changed 🛠
- [PR#587](https://github.com/EmbarkStudios/rust-gpu/pull/587) updated `glam` to `0.14`
- [PR#605](https://github.com/EmbarkStudios/rust-gpu/pull/605) updated toolchain to `nightly-2021-04-25`

### Fixed 🩹
- [PR#594](https://github.com/EmbarkStudios/rust-gpu/pull/594) fixed [#585](https://github.com/EmbarkStudios/rust-gpu/issues/585) by explicitly banning `Image`/`Sampler`/`SampledImage` entry-point parameters not behind references
- [PR#598](https://github.com/EmbarkStudios/rust-gpu/pull/598) fixed [#581](https://github.com/EmbarkStudios/rust-gpu/issues/581) by switching `memory::Semantics` from an `enum` to a `bitflags!`, in `spirv-std`

## [0.4.0-alpha.5]

### Removed 🔥
- [PR#583](https://github.com/EmbarkStudios/rust-gpu/pull/583) removed `memcmp` from `spirv-std`

## [0.4.0-alpha.4]

### Added ⭐
- [PR#519](https://github.com/EmbarkStudios/rust-gpu/pull/519) added `memory_barrier` and `control_barrier` to `spirv-std`

### Changed 🛠
- [PR#567](https://github.com/EmbarkStudios/rust-gpu/pull/567) removed the need to manually specify the storage class for `Image`/`Sampler`/`ImageSampler` entry-point parameters

### Deprecated 🚧
- [PR#576](https://github.com/EmbarkStudios/rust-gpu/pull/576) deprecated `#[spirv(block)]` in favor of automatically wrapping the user types in "interface blocks"

## [0.4.0-alpha.3]

### Added ⭐
- [PR#551](https://github.com/EmbarkStudios/rust-gpu/pull/551) added multi-module (one SPIR-V module per entry-point) support to `spirv-builder`
- [PR#504](https://github.com/EmbarkStudios/rust-gpu/pull/504) added basic support for unsized `struct`s (e.g. ending with a `[T]` field)
- [PR#545](https://github.com/EmbarkStudios/rust-gpu/pull/545) added `Image` methods for sampling depth reference and/or with project coordinate, to `spirv-std`

## [0.4.0-alpha.2]

### Added ⭐
- [PR#541](https://github.com/EmbarkStudios/rust-gpu/pull/541) added `#[spirv(invariant)]` (like the `invariant` keyword in GLSL)

### Fixed 🩹
- made `arch::derivative` functions public, in `spirv-std`

## [0.4.0-alpha.1]

### Added ⭐
- [PR#498](https://github.com/EmbarkStudios/rust-gpu/pull/498) added `sample_by_lod`/`sample_by_gradient` image methods to `spirv-std`
- [PR#521](https://github.com/EmbarkStudios/rust-gpu/pull/521) added `Cubemap` to `spirv-std`
- [PR#520](https://github.com/EmbarkStudios/rust-gpu/pull/520) added `arch::primitive` functions to `spirv-std`

### Changed 🛠
- [PR#496](https://github.com/EmbarkStudios/rust-gpu/pull/496) updated `spirv-tools` to `0.5.0`
- [PR#516](https://github.com/EmbarkStudios/rust-gpu/pull/516) updated toolchain to `nightly-2021-03-21`
- [PR#443](https://github.com/EmbarkStudios/rust-gpu/pull/443) replaced `spirv_std::storage_class` "named pointer types" with `#[spirv(...)] &T` entry-point parameters

## [0.3.1]

### Added ⭐
- [PR#480](https://github.com/EmbarkStudios/rust-gpu/pull/480) added a `fetch` image method to `spirv-std`
- [PR#446](https://github.com/EmbarkStudios/rust-gpu/pull/446) added `arch::*` functions for all SPIR-V arithmetic operations (not involving matrices), to `spirv-std`

### Removed 🔥
- [PR#476](https://github.com/EmbarkStudios/rust-gpu/pull/476) removed `glam` as a dependency of `spirv-std`

## [0.3.0]

### Added ⭐
- [PR#414](https://github.com/EmbarkStudios/rust-gpu/pull/414) added storage class type inference
- [PR#469](https://github.com/EmbarkStudios/rust-gpu/pull/469) added initial support for Algebraic Data Type enums (e.g. `Option<T>`)
- [PR#421](https://github.com/EmbarkStudios/rust-gpu/pull/421) added ability to provide `const` arguments to `asm!`
- [PR#458](https://github.com/EmbarkStudios/rust-gpu/pull/458) added ability to set `entry_point_name` in entry point attributes to change the final name of an entry point
- [PR#337](https://github.com/EmbarkStudios/rust-gpu/pull/337) added `#[spirv(unroll_loops)]` attribute to functions, which tells `rustc_codegen_spirv` to annotate all loops inside with `Unroll`
- [PR#394](https://github.com/EmbarkStudios/rust-gpu/pull/394) added a new [`arch`] module which provides an abstraction some basic SPIR-V instructions as free functions.
- [PR#340](https://github.com/EmbarkStudios/rust-gpu/pull/340) added the `spirv-std-macros` crate for holding the `spirv` proc macro
- [PR#392](https://github.com/EmbarkStudios/rust-gpu/pull/392) added the `gpu_only` proc macro
- [PR#377](https://github.com/EmbarkStudios/rust-gpu/pull/377) `Derivative` is now implemented for `glam::{Vec2, Vec3, Vec3A, Vec4}`
- [PR#401](https://github.com/EmbarkStudios/rust-gpu/pull/401) added ability to build shaders in `release` mode
- new `spirv-std` APIs: `vector_extract_dynamic`<sup>[PR#394](https://github.com/EmbarkStudios/rust-gpu/pull/394)</sup>, `vector_insert_dynamic`<sup>[PR#411](https://github.com/EmbarkStudios/rust-gpu/pull/411)</sup>, `textures::StorageImage2d`<sup>[PR#434](https://github.com/EmbarkStudios/rust-gpu/pull/434)</sup>, `any`/`all`<sup>[PR#380](https://github.com/EmbarkStudios/rust-gpu/pull/441)</sup>, `discard`<sup>[PR#441](https://github.com/EmbarkStudios/rust-gpu/pull/380)</sup>, `demote_to_helper_invocation`<sup>[PR#380](https://github.com/EmbarkStudios/rust-gpu/pull/380)</sup>, ``SampledImage``<sup>[PR#320](https://github.com/EmbarkStudios/rust-gpu/pull/320)</sup>

### Changed 🛠
- [PR#461](https://github.com/EmbarkStudios/rust-gpu/pull/461) removed requirement of `#[allow(unused_attributes)]` in front of `#[spirv]` attributes to remove warnings
- [PR#398](https://github.com/EmbarkStudios/rust-gpu/pull/398) `rustc_codegen_spirv` now removes different `OpName`s that target the same ID
- [PR#396](https://github.com/EmbarkStudios/rust-gpu/pull/396) `rustc_codegen_spirv` now tries to deduplicate generated `OpVariable`s

## [0.2.0]

### Added ⭐
- [PR#287](https://github.com/EmbarkStudios/rust-gpu/pull/287) added a new structurizer, which means that you can now use `match` expressions and `continue`s
- [PR#317](https://github.com/EmbarkStudios/rust-gpu/pull/317) added the `#[spirv(flat)]` attribute that matches SPIR-V's "Flat" decorator.
- [PR#276](https://github.com/EmbarkStudios/rust-gpu/pull/276) added support for textures.
- [PR#305](https://github.com/EmbarkStudios/rust-gpu/pull/305) added support for `panic!`
- [PR#165](https://github.com/EmbarkStudios/rust-gpu/pull/165) added support for SPIR-V `1.0`
- [PR#268](https://github.com/EmbarkStudios/rust-gpu/pull/268) added support for procedural macros
- [PR#195](https://github.com/EmbarkStudios/rust-gpu/pull/195) added initial support for  compute shaders
- [PR#254](https://github.com/EmbarkStudios/rust-gpu/pull/254) added initial support in Rust and `rust-gpu` for inline SPIR-V with the `asm!` nightly feature

### Changed 🛠
- [PR#219](https://github.com/EmbarkStudios/rust-gpu/pull/219) improvements to error messages regarding constant pointers
- [PR#280](https://github.com/EmbarkStudios/rust-gpu/pull/280) all Storage Classes (e.g. `Input`/`Output`) are now defined in `spirv_std::storage_class`
- [PR#275](https://github.com/EmbarkStudios/rust-gpu/pull/275) Rust's language items such `rust_eh_personality` and `panic_handler` are now defined in `spirv-std` for SPIR-V targets

## [0.1.0]

Initial release.
