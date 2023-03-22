#[cfg(target_os = "android")]
const _: () = panic!(
    "executable not applicable for Android targets, \
     make sure to pass `--lib` when building with `cargo-apk`"
);

fn main() {
    #[cfg(not(target_os = "android"))]
    example_runner_wgpu::main();
}
