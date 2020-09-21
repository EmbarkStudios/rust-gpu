#!/usr/bin/env bash

# exit on cmd failure
set -e

# build rustc_codegen_spirv
cargo build

export RUSTFLAGS="-Zcodegen-backend=$PWD/../target/debug/librustc_codegen_spirv.so -Ccodegen-units=1"

pushd build_libcore_test
cargo build -Z build-std=core --target spirv-unknown-unknown --release
popd
