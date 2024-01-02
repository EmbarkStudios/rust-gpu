#!/usr/bin/env bash
set -e

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

# HACK(eddyb) build docs using one toolchain, otherwise they wipe eachother.
ORIG_DIR="$PWD"
TOOLCHAIN_DIR="crates/rustc_codegen_spirv"
function doc() {
    local crate_path="$1"
    shift

    echo ::group::"$crate_path"
    # HACK(eddyb) need to be in `crates/rustc_codegen_spirv` to use its toolchain.
    (
        cd "$TOOLCHAIN_DIR"
        cargo doc \
            --manifest-path "$ORIG_DIR/$crate_path/Cargo.toml" \
            --target-dir "$ORIG_DIR/target" \
            "$@"
    )
    echo ::endgroup::
}
function doc_with_host_feat() {
    doc "$@" --no-default-features --features "$FEAT"
}

# Core crates only!
doc crates/spirv-std --all-features
doc_with_host_feat crates/rustc_codegen_spirv
doc_with_host_feat crates/spirv-builder
