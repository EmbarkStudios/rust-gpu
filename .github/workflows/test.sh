#!/usr/bin/env bash
set -ex

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

os=$1

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

function cargo_test_no_features() {
    echo ::group::"$1 build"
    cargo test --manifest-path "$1/Cargo.toml" --no-run
    echo ::endgroup::

    echo ::group::"$1 test"
    cargo test --manifest-path "$1/Cargo.toml"
    echo ::endgroup::
}

# Core crates
cargo_test spirv-tools-sys
cargo_test spirv-tools
cargo_test rustc_codegen_spirv
cargo_test spirv-builder

# Examples
# See: https://github.com/EmbarkStudios/rust-gpu/issues/84
if [[ -z "${CI}" && "$os" != "macOS" ]]; then
    cargo_test examples/example-runner
fi

cargo_test examples/wgpu-example-runner

cargo_test_no_features examples/example-runner-cpu
cargo_test_no_features examples/example-shader
cargo_test_no_features examples/wgpu-example-shader
