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
    (check out ["codegen args" docs](docs/src/codegen-args.md), or run with `RUSTGPU_CODEGEN_ARGS=--help` to see the full list of options)

### Changed 🛠️
- [PR#958](https://github.com/EmbarkStudios/rust-gpu/pull/958) updated toolchain to `nightly-2022-10-29`
- [PR#941](https://github.com/EmbarkStudios/rust-gpu/pull/941) applied workspace inheritance to `Cargo.toml` files
- [PR#959](https://github.com/EmbarkStudios/rust-gpu/pull/959) moved `rustc_codegen_spirv` debugging functionality from environment variables to "codegen args" options/flags (see [the updated docs](docs/src/codegen-args.md) for more details)

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
