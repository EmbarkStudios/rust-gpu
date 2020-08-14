setlocal
cargo build

set RUSTFLAGS=-Zcodegen-backend=%cd%/target/debug/rustc_codegen_spirv.dll

pushd build_libcore_test
xargo build --target wasm32-unknown-unknown
popd
