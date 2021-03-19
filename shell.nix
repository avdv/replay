{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.ormolu
    bazel-watcher
    bazel-buildtools
  ];
}
