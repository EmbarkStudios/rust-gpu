let
  pkgs = import <nixpkgs> {};
in with pkgs; stdenv.mkDerivation rec {
  name = "rust-gpu";

  # Workaround for https://github.com/NixOS/nixpkgs/issues/60919.
  hardeningDisable = [ "fortify" ];

  # Allow cargo to download crates.
  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  buildInputs = [
    pkgconfig rustup x11 libxkbcommon
  ];

  # Runtime dependencies.
  LD_LIBRARY_PATH = with xlibs; lib.makeLibraryPath [
    libX11 libXcursor libXi libXrandr vulkan-loader
  ];
}
