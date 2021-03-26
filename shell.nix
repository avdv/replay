{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    stylish-haskell
    bazel-watcher
    bazel-buildtools
  ];
}
