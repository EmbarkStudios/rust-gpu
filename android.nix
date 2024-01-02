# HACK(eddyb) this is a minimal setup to allow testing the Android build on NixOS.
#
# Tested in `NIXPKGS_ACCEPT_ANDROID_SDK_LICENSE=1 nix-shell android.nix --pure`,
# by running the following commands (x64 target is for the Android Emulator):
#   rustup target add aarch64-linux-android x86_64-linux-android
#   cargo apk build -p example-runner-wgpu --lib --target aarch64-linux-android
#   cargo apk build -p example-runner-wgpu --lib --target x86_64-linux-android
#
# (you can also replace `cargo apk build` with `cargo apk run` to launch it,
# via `adb`, into either the Android Emulator, or a physical Android device)

let
  pkgs = import <nixpkgs> {};
in with pkgs; mkShell rec {
  # Workaround for https://github.com/NixOS/nixpkgs/issues/60919.
  # NOTE(eddyb) needed only in debug mode (warnings about needing optimizations
  # turn into errors due to `-Werror`, for at least `spirv-tools-sys`).
  hardeningDisable = [ "fortify" ];

  # Allow cargo to download crates (even inside `nix-shell --pure`).
  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  nativeBuildInputs = [ rustup cargo-apk jdk ];

  ANDROID_SDK_ROOT = let
    androidComposition = androidenv.composeAndroidPackages {
      abiVersions = [ "arm64-v8a" "x86_64" ];
      includeNDK = true;
      platformVersions = [ "30" ];
    };
  in "${androidComposition.androidsdk}/libexec/android-sdk";

  ANDROID_NDK_ROOT = "${ANDROID_SDK_ROOT}/ndk-bundle";
}
