# Testing Rust-GPU

Rust-GPU has a couple of different kinds of tests, most can be ran through
`cargo test`, however Rust-GPU also has end-to-end tests for compiling Rust and
validating its SPIR-V output, which can ran by running `cargo compiletest`.

```bash
cargo test && cargo compiletest
```

## Adding Tests

Rust-GPU's end-to-end test's use an external version of the [`compiletest`] tool
as a testing framework. Be sure to check out the [repository][`compiletest`] and
the [rustc Dev-Guide][rustc-dev-guide] for more information about how it works,
how to configure it, and add new tests.

### Blessing Tests

You will occassionally need to "bless" the output from UI tests to update the
normalised output, you can do this by passing a `--bless` flag to
`cargo compiletest`.

``
cargo compiletest -- --bless
``

### Caching Tests

`cargo compiletest` by default caches the build output for dependencies of test
cases such as `spirv-std` and `glam`. If you need to compile from a fresh source
you can pass the `--clean` flag to build from scratch.

``
cargo compiletest -- --clean
``

[`compiletest`]: https://github.com/laumann/compiletest-rs
[rustc-dev-guide]: https://rustc-dev-guide.rust-lang.org/tests/intro.html
