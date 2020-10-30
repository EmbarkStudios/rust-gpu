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
clippy spirv-tools-sys
clippy spirv-tools
clippy rustc_codegen_spirv
clippy spirv-builder

# Examples

clippy examples/runners/ash
clippy examples/runners/wgpu

clippy_no_features examples/runners/cpu
# disabled due to https://github.com/EmbarkStudios/rust-gpu/issues/186
# clippy_no_features examples/shaders/sky-shader
clippy_no_features examples/shaders/simplest-shader
