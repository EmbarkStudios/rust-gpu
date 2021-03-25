#!/usr/bin/env bash
set -e

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

os=$1

function cargo_build() {
    echo ::group::"$1 build"
    cargo build \
        --manifest-path "$1/Cargo.toml" \
        --no-default-features \
        --features "$FEAT"
    echo ::endgroup::
}

function cargo_build_no_features() {
    echo ::group::"$1 build"
    cargo build --manifest-path "$1/Cargo.toml"
    echo ::endgroup::
}

function cargo_test() {
    echo ::group::"$1 build"
    cargo test \
        --manifest-path "$1/Cargo.toml" \
        --no-default-features \
        --features "$FEAT" \
        --no-run
    echo ::endgroup::

    echo ::group::"$1 test"
    cargo test \
        --manifest-path "$1/Cargo.toml" \
        --no-default-features \
        --features "$FEAT"
    echo ::endgroup::
}

# Core crates
cargo_test crates/rustc_codegen_spirv
cargo_test crates/spirv-builder

# Run compiletest test suite
CARGO_FEATURES="$FEAT" && CARGO_DEFAULT_FEATURES="false" && cargo compiletest

# Test compiling examples.
cargo_build examples/runners/ash
cargo_build examples/runners/wgpu
cargo_build examples/runners/cpu

# Test compiling shaders on the CPU.
cargo_build_no_features examples/shaders/sky-shader
cargo_build_no_features examples/shaders/simplest-shader

