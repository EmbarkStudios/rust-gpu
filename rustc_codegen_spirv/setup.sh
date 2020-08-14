#!/usr/bin/env bash
rustup component add rust-src rustc-dev llvm-tools-preview
rustup target add wasm32-unknown-unknown
