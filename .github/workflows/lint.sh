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
clippy crates/rustc_codegen_spirv
clippy crates/spirv-builder

# Examples

clippy examples/runners/ash
clippy examples/runners/wgpu

clippy_no_features examples/runners/cpu
clippy_no_features examples/shaders/sky-shader
clippy_no_features examples/shaders/simplest-shader

# Custom lints

# 1. Disallow `std::env` (mis)use from `rustc_codegen_spirv`

# HACK(eddyb) see `docs/src/codegen-args.md` for more context around this,
# but basically we're implementing a custom "lint" to ban `std::env` usage,
# which could be disastrous because env vars access can't be tracked by
# `rustc`, unlike its CLI flags (which are integrated with incremental).
if (
    egrep -r '::\s*env|env\s*::' crates/rustc_codegen_spirv/src/ |
    # HACK(eddyb) exclude the one place in `rustc_codegen_spirv`
    # needing access to an env var (only for codegen args `--help`).
    egrep -v '^crates/rustc_codegen_spirv/src/codegen_cx/mod.rs:            let help_flag_comes_from_spirv_builder_env_var = std::env::var\(spirv_builder_env_var\)$'
); then
    echo '^^^'
    echo '!!! Found disallowed `std::env` usage in `rustc_codegen_spirv` !!!'
    echo ' ("codegen args" should be used instead of environment variables)'
    echo
    echo 'For more details on "codegen args", see: docs/src/codegen-args.md'
    echo ' (and/or https://github.com/EmbarkStudios/rust-gpu/pull/959)'
    echo
    exit 1
fi
