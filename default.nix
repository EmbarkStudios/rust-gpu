let
  pkgs = import <nixpkgs> {};
in with pkgs; stdenv.mkDerivation rec {
  name = "rust-gpu";

  # Workaround for https://github.com/NixOS/nixpkgs/issues/60919.
  hardeningDisable = [ "fortify" ];

  # Allow cargo to download crates.
  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  buildInputs = [
    pkgconfig rustup xlibsWrapper libxkbcommon
  ];

  # Runtime dependencies.
  LD_LIBRARY_PATH = with xorg; lib.makeLibraryPath [
    vulkan-loader

    # NOTE(eddyb) winit really wants `libxkbcommon` on Wayland for some reason
    # (see https://github.com/rust-windowing/winit/issues/1760 for more info).
    wayland libxkbcommon

    libX11 libXcursor libXi libXrandr
  ];
}
