#!/usr/bin/env bash
set -e

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

function clippy() {
    echo ::group::"$1"
    cargo clippy \
        --manifest-path "$1/Cargo.toml" \
        --no-default-features \
        --features "$FEAT" \
        --all-targets \
        -- -D warnings
    echo ::endgroup::
}

function clippy_no_features() {
    echo ::group::"$1"
    cargo clippy \
        --manifest-path "$1/Cargo.toml" \
        --all-targets \
        -- -D warnings
    echo ::endgroup::
}

# Core crates
clippy crates/spirv-tools-sys
clippy crates/spirv-tools
clippy crates/rustc_codegen_spirv
clippy crates/spirv-builder
clippy crates/testsuite

# Examples

clippy examples/runners/ash
clippy examples/runners/wgpu

clippy_no_features examples/runners/cpu
clippy_no_features examples/shaders/sky-shader
clippy_no_features examples/shaders/simplest-shader
