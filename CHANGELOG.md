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

- Added two `spirv-builder` environment variables to customize *only* the `rustc` invocations for shader crates and their dependencies:
    - `RUSTGPU_RUSTFLAGS="..."` for shader `RUSTFLAGS="..."`
    - `RUSTGPU_CODEGEN_ARGS="..."` for shader "codegen args" (i.e. `RUSTFLAGS=-Cllvm-args="..."`)  
      (check out ["codegen args" docs](docs/src/codegen-args.md), or run with `RUSTGPU_CODEGEN_ARGS=--help` to see the full list of options)

### Changed 🛠️

- Updated toolchain to `nightly-2022-10-29`
- Applied workspace inheritance to `Cargo.toml` files
- Moved `rustc_codegen_spirv` debugging functionality from environment variables to "codegen args" options/flags (see [the updated docs](docs/src/codegen-args.md) for more details)

### Removed 🔥

- Removed the `fn`/closure `#[spirv(unroll_loops)]` attribute, as it has no users,
  is becoming non-trivial to support, and requires redesign for better ergonomics
  (e.g. `#[spirv(unroll)]` applied to individual loops, not the whole `fn`/closure)

## [0.4.0-alpha.17]

### Changed 🛠️

- Fixed rust-gpu crates not referring to each-other by exact version
- Fixed `spirv-std` referring to an older version of `spirv-std-macros`

## [0.4.0-alpha.16]

### Added ⭐

- Added check for environment variable `RUSTGPU_SKIP_TOOLCHAIN_CHECK` to prevent toolchain check

### Changed 🛠️

- 🚨BREAKING🚨 Migrated from `register_attr` to `register_tool`. [More information](docs/src/migration-to-register-tool.md).
- Updated toolchain to `nightly-2022-10-01`
- Updated `glam` to `0.22`

### Removed 🔥

- Removed `glam::BVec` support (they are no longer `#[repl(simd)]` in `glam`, as Rust doesn't support SIMD vectors with `bool` elements)

## [0.4.0-alpha.15]

### Added ⭐

- Build-time check for nightly toolchain version to provide user-friendly error messages.

### Changed 🛠️

- Updated rust toolchain to `nightly-2022-08-29`.
