// Putting this check here causes compilation failure seconds into the build,
// putting it in lib.rs fails after minutes because spirv-tools gets compiled first.
#[cfg(all(feature = "use-compiled-tools", feature = "use-installed-tools"))]
compile_error!("Either \"use-compiled-tools\" (enabled by default) or \"use-installed-tools\" may be enabled.");

fn main() {}