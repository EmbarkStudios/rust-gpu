setlocal

(set /p nightly=)<nightly

echo %nightly%

rustup install %nightly%
rustup default %nightly%
rustup component add rust-src rustc-dev llvm-tools-preview
rustup target add wasm32-unknown-unknown
