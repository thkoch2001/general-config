# channel is a separate input config tied utility, but its heavily
# tied to river
{ pkgs ? import <nixpkgs> {}, ... }:
let
  version = "0.4.1";
in
pkgs.stdenv.mkDerivation (final: {
  pname = "channel";
  version = version;
  deps = pkgs.callPackage ./channel.zon.nix { };

  nativeBuildInputs = with pkgs; [
    pkg-config
    wayland-scanner
    zig_0_16
  ];

  buildInputs = with pkgs; [
    libxkbcommon
    wayland
    wayland-protocols
    river
  ];

  zigBuildFlags = [
    "--system"
    "${final.deps}"
  ];

  src = pkgs.fetchFromCodeberg {
    owner = "Sivecano";
    repo = "channel";
    rev = version;
    hash = "sha256-AIP5SO7p2Z8fYeLSoIliSya2fQALV1xkkYfIRdHY6TQ=";
  };
})
