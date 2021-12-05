{ sources ? import ./sources.nix
}:
let
  # default nixpkgs
  pkgs = import sources.nixpkgs { };

  # gitignore.nix
  gitignoreSource = (import sources."gitignore.nix" { inherit (pkgs) lib; }).gitignoreSource;

  src = gitignoreSource ./..;
in
{
  inherit pkgs src;

  # provided by shell.nix
  devTools = {
    inherit (pkgs) niv pre-commit bazel_4 bazel-watcher bazel-buildtools openjdk11_headless stylish-haskell haskell-language-server;

    ghc = pkgs.haskell.packages.ghc8107.ghc;
  };

  # to be built by github actions
  ci = {
    pre-commit-check = (import sources."pre-commit-hooks.nix").run {
      inherit src;
      hooks = {
        shellcheck.enable = true;
        nixpkgs-fmt.enable = true;
        nix-linter.enable = true;
        stylish-haskell.enable = true;
        buildifier = {
          enable = true;

          name = "buildifier";

          entry = "${pkgs.buildifier}/bin/buildifier -mode=fix -lint=fix";

          files = "^(WORKSPACE|BUILD([.]bazel)?|.+[.]bzl)$";

          # List of file types to run on (default: [ "file" ] (all files))
          # see also https://pre-commit.com/#filtering-files-with-types
          # You probably only need to specify one of `files` or `types`:
          #types = [ "text" "c" ];
        };
      };
      # generated files / submodules
      excludes = [
        "^nix/sources\.nix$"
        "^modules/"
      ];
    };
  };
}
