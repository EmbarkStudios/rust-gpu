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

# 2. Ensure `spirv-std` & helper crates have necessary dependency versions
#    listed in their `Cargo.toml` files, by using `cargo -Z minimal-versions`.
echo 'Testing dependency versions with `cargo -Z minimal-versions`:'

function version_test() {
    local crate_path="$1"
    local crate_name="$(basename "$crate_path")"
    local test_dir="$(mktemp -d --tmpdir "version-test-$crate_name-XXXXXXXXXX")"
    local test_cargoflags=(
        -Z minimal-versions
        --manifest-path "$test_dir/Cargo.toml"
    )

    echo ::group::"$crate_name (via $test_dir)"
    cargo init --lib --vcs none --name "version-test-$crate_name" "$test_dir"
    cargo add "${test_cargoflags[@]}" --path "$crate_path"
    cargo clippy "${test_cargoflags[@]}" -- -D warnings
    rm -r "$test_dir"
    echo ::endgroup::
}
# FIXME(eddyb) try to get this working for `spirv-builder`, which has a larger
# dependency graph, with too much imprecision in upstream `Cargo.toml` files.
version_test crates/spirv-std
