{ pkgs ? import ../nixpkgs.nix { } }:

pkgs.haskellPackages.ghcWithPackages (hs: with hs; [
  brick
  hinotify
  hspec
  optparse-applicative
  process
  unix
])
