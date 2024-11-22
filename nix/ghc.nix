{ pkgs ? import <nixpkgs> { }, ghcVersion, withHoogle ? false }:

let
  nixGhcVersion = builtins.replaceStrings [ "." ] [ "" ] ghcVersion;
  ghc = pkgs.pkgs.haskell.packages."ghc${nixGhcVersion}";
  withPackages = if withHoogle then ghc.ghcWithHoogle else ghc.ghcWithPackages;
in
withPackages (hs: with hs; [
  auto-update
  brick
  fsnotify
  hspec
  ini
  optparse-applicative
  process
  unix
])
