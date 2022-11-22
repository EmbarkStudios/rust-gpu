# `rust-gpu` Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed 🛠️

- Applied workspace inheritance to Cargo.toml files

## [0.4.0-alpha.17]

### Changed 🛠️

- Fixed rust-gpu crates not referring to each-other by exact version
- Fixed `spirv-std` referring to an older version of `spirv-std-macros`

## [0.4.0-alpha.16]

### Added ⭐

- Added check for env var `RUSTGPU_SKIP_TOOLCHAIN_CHECK` to prevent toolchain check

### Changed 🛠️

- 🚨BREAKING🚨 Migrated from `register_attr` to `register_tool`. [More information](docs/src/migration-to-register-tool.md).
- Updated toolchain to `nightly-2022-10-01`
- Updated `glam` to `0.22`
- Removed `glam::BVec` support (they are no longer `#[repl(simd)]` in `glam`, as Rust doesn't support SIMD vectors with `bool` elements)

## [0.4.0-alpha.15]

### Added ⭐

- Build-time check for nightly toolchain version to provide user-friendly error messages.

### Changed 🛠️

- Updated rust toolchain to `nightly-2022-08-29`.
