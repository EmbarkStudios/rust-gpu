#!/usr/bin/env bash
set -e

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

os=$1

function cargo_test() {
    echo ::group::"$1 build"
    cargo build \
        --manifest-path "$1/Cargo.toml" \
        --no-default-features \
        --features "$FEAT" \
        --tests
    echo ::endgroup::

    echo ::group::"$1 test"
    cargo test \
        --manifest-path "$1/Cargo.toml" \
        --no-default-features \
        --features "$FEAT"
    echo ::endgroup::
}

function cargo_test_no_features() {
    echo ::group::"$1 build"
    cargo build --manifest-path $1/Cargo.toml
    echo ::endgroup::

    echo ::group::"$1 test"
    cargo test --manifest-path $1/Cargo.toml
    echo ::endgroup::
}

# # Core crates
cargo_test_no_features crates/rustc_codegen_spirv
cargo_test_no_features crates/spirv-builder
cargo_test_no_features crates/spirv-std

# Examples
# See: https://github.com/EmbarkStudios/rust-gpu/issues/84
if [[ "$os" != "macOS" ]]; then
    cargo_test_no_features examples/runners/ash
fi

cargo_test_no_features examples/runners/wgpu
cargo_test_no_features examples/runners/cpu
cargo_test_no_features examples/shaders/sky-shader
cargo_test_no_features examples/shaders/simplest-shader
