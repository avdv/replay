{ pkgs ? import ../nixpkgs.nix { } }:

pkgs.haskell.packages.ghc922.ghcWithPackages (hs: with hs; [
  brick
  hinotify
  hspec
  optparse-applicative
  process
  unix
])
