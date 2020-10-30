#!/usr/bin/env bash
set -e

if [[ -z "${CI}" ]]; then
    FEAT="use-compiled-tools"
else
    FEAT="use-installed-tools"
fi

function doc() {
    echo ::group::"$1"
    cargo doc \
        --manifest-path "$1/Cargo.toml" \
        --no-default-features \
        --features "$FEAT"
    echo ::endgroup::
}

# Core crates only!
doc spirv-std
doc spirv-tools-sys
doc spirv-tools
doc rustc_codegen_spirv
doc spirv-builder
