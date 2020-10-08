setlocal
cargo build

set RUSTFLAGS=-Zcodegen-backend=%cd%/../target/debug/rustc_codegen_spirv.dll

pushd build_libcore_test
set SPIRV_VAL=1
cargo build -Z build-std=core --target spirv-unknown-unknown --release
popd
