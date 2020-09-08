#!/usr/bin/env bash

# exit on cmd failure
set -e

# build rustc_codegen_spirv
cargo build

export RUSTFLAGS=-Zcodegen-backend=$PWD/target/debug/librustc_codegen_spirv.so

pushd build_libcore_test
# Use wasm32 because it's a relatively simple platform - if the x86 libcore is used, there's all sorts of "feature sse2
# not found" and the like, and our spirv backend is never reached. With wasm32, it at least gets reached.
# (We probably want to add our own target eventually)
xargo build --target wasm32-unknown-unknown
popd
