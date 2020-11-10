#!/usr/bin/env bash

rustup toolchain install nightly-2020-11-09 --component rust-src rustc-dev llvm-tools-preview

git submodule init
git submodule update
