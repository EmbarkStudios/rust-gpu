#!/usr/bin/env bash
rustc +nightly -Zcodegen-backend=target/debug/librustc_codegen_spirv.so --crate-type lib test/empty.rs
