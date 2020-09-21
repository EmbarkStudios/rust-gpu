setlocal
cargo build

set RUSTFLAGS=-Zcodegen-backend=%cd%/../target/debug/rustc_codegen_spirv.dll -Ccodegen-units=1

pushd build_libcore_test
cargo build -Z build-std=core --target spirv-unknown-unknown --release
popd
