{ pkgs ? import <nixpkgs> {}, ghcVersion ? "ghc844" }:

pkgs.haskell.lib.buildStackProject {
  pname = "my-xmonad";
  version = "0.1.0.0";
  buildInputs = with pkgs; [
    xorg.libX11
    xorg.libXrandr
    xorg.libXScrnSaver
    xorg.libXext
    xorg.libXft
  ];
}
