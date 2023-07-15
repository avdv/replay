{ pkgs ? import <nixpkgs> { }, ghcVersion }:

pkgs.pkgs.haskell.packages."ghc${builtins.replaceStrings  ["."] [""] ghcVersion}".ghcWithPackages (hs: with hs; [
  brick
  hinotify
  hspec
  optparse-applicative
  process
  unix
])
