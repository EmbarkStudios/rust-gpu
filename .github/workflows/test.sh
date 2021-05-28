#!/usr/bin/env bash
set -e

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

# Core crates
# Compiled in --release because cargo compiletest would otherwise compile in release again.
echo ::group::rustc_codegen_spirv build
cargo test \
    -p rustc_codegen_spirv \
    --release \
    --no-default-features \
    --features "$FEAT" \
    --no-run
echo ::endgroup::

echo ::group::rustc_codegen_spirv test
cargo test \
    -p rustc_codegen_spirv \
    --release \
    --no-default-features \
    --features "$FEAT"
echo ::endgroup::

echo ::group::compiletest
cargo run \
    -p compiletests \
    --release \
    --no-default-features \
    --features "$FEAT" \
    -- \
    --target-env vulkan1.1,spv1.3
echo ::endgroup::

# Examples
echo ::group::cargo check examples
cargo check \
    -p example-runner-ash \
    -p example-runner-wgpu \
    -p example-runner-cpu \
    -p compute-shader \
    -p mouse-shader \
    -p simplest-shader \
    -p sky-shader \
    --no-default-features \
    --features "$FEAT"
echo ::endgroup::

echo ::group::build example shaders
OUT_DIR=target/tmp cargo run \
    -p example-runner-wgpu-builder \
    --release \
    --no-default-features \
    --features "$FEAT"
echo ::endgroup::
