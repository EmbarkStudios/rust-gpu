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
clippy examples/example-runner
clippy examples/wgpu-example-runner

clippy_no_features examples/example-runner-cpu
clippy_no_features examples/example-shader
clippy_no_features examples/wgpu-example-shader