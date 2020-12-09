setlocal

rem HACK(eddyb) including `rust-std` works around this `rustup` bug:
rem https://github.com/rust-lang/rustup/issues/2601

rustup component add rust-std rust-src rustc-dev llvm-tools-preview
