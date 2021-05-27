#!/usr/bin/env bash
set -e

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

# Core crates
# Compiled in --release because cargo compiletest would otherwise compile in release again.
time {
    echo ::group::rustc_codegen_spirv build
    cargo -v test \
        -p rustc_codegen_spirv \
        --release \
        --no-default-features \
        --features "$FEAT" \
        --no-run
    echo ::endgroup::
}

time {
    echo ::group::rustc_codegen_spirv test
    cargo -v test \
        -p rustc_codegen_spirv \
        --release \
        --no-default-features \
        --features "$FEAT"
    echo ::endgroup::
}

time {
    echo ::group::compiletest
    cargo -v run \
        -p compiletests \
        --release \
        --no-default-features \
        --features "$FEAT" \
        -- \
        --target-env vulkan1.1,spv1.3
    echo ::endgroup::
}

# Examples
time {
    echo ::group::cargo check examples
    cargo -v check \
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
}

time {
    echo ::group::build example shaders
    OUT_DIR=target/tmp cargo -v run \
        -p example-runner-wgpu-builder \
        --release \
        --no-default-features \
        --features "$FEAT"
    echo ::endgroup::
}
