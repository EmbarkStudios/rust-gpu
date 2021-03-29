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

```bash
cargo compiletest --bless
```

### Filtering Tests

When working on tests, you may need to run `cargo compiletest` a few times,
while changing only a small number of tests. You can avoid having to run all
the other (unrelated) tests, by passing substrings of their paths, to
`cargo compiletest`, for example:

```bash
cargo compiletest arch/u image
```

The above command will only test `ui/arch/u_*.rs` and `ui/image/*.rs`, and skip
everything else. You can also add `--bless` to update expected outputs, as well.

### Testing Different Environments

You can test against multiple different SPIR-V environments with the
`--target-env` flag. By default it is set to `unknown`.

```bash
cargo compiletest --target-env=vulkan1.1
# You can also provide multiple values to test multiple environments
cargo compiletest --target-env=vulkan1.1,spv.1.3
```

[`compiletest`]: https://github.com/laumann/compiletest-rs
[rustc-dev-guide]: https://rustc-dev-guide.rust-lang.org/tests/intro.html
