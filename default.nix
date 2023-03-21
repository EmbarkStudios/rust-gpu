let
  pkgs = import <nixpkgs> {};
in with pkgs; stdenv.mkDerivation rec {
  name = "rust-gpu";

  # Workaround for https://github.com/NixOS/nixpkgs/issues/60919.
  # NOTE(eddyb) needed only in debug mode (warnings about needing optimizations
  # turn into errors due to `-Werror`, for at least `spirv-tools-sys`).
  hardeningDisable = [ "fortify" ];

  # Allow cargo to download crates (even inside `nix-shell --pure`).
  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  nativeBuildInputs = [ rustup ];

  # Runtime dependencies (for the example runners).
  LD_LIBRARY_PATH = with xorg; lib.makeLibraryPath [
    vulkan-loader

    # NOTE(eddyb) winit really wants `libxkbcommon` on Wayland for some reason
    # (see https://github.com/rust-windowing/winit/issues/1760 for more info).
    wayland libxkbcommon

    libX11 libXcursor libXi libXrandr
  ];
}
