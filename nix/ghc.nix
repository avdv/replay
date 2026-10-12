{
  pkgs ? import <nixpkgs> { },
  ghcVersion,
  withHoogle ? false,
}:

let
  nixGhcVersion = builtins.replaceStrings [ "." ] [ "" ] ghcVersion;
  ghc = pkgs.pkgs.haskell.packages."ghc${nixGhcVersion}";
  withPackages = if withHoogle then ghc.ghcWithHoogle else ghc.ghcWithPackages;
in
withPackages (
  hs: with hs; [
    attoparsec
    auto-update
    brick
    fsnotify
    hspec
    ini
    json
    microlens
    microlens-th
    optparse-applicative
    process
    unix
    vty-unix
  ]
)
