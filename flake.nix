{
  description = "scalals";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { nixpkgs, flake-utils, pre-commit-hooks, ... }:
    with flake-utils.lib.system;
    flake-utils.lib.eachSystem [ aarch64-linux x86_64-darwin x86_64-linux ]
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          bazel = pkgs.bazel_4;
          inherit (pkgs) bazel-watcher;
          ghc = import ./nix/ghc.nix { inherit pkgs; };
          nativeBuildInputs = with pkgs; [
            git
            ghc
          ];
          devTools = with pkgs; [
            bazel
            haskell-language-server
          ] ++ lib.optional (!bazel-watcher.meta.broken) bazel-watcher;
        in
        rec {
          packages.replay = pkgs.buildBazelPackage {
            name = "replay-0.2.0";
            src = ./.;

            removeRulesCC = false;

            bazelBuildFlags = [
              "--compilation_mode=opt" # optimize
              "--verbose_failures"
            ];

            passthru = {
              exePath = "/bin/replay";
            };

            bazelTarget = "//:replay";

            inherit bazel nativeBuildInputs;

            fetchAttrs = {
              sha256 = "sha256-ncmHm3ccz4zDpjz63kD3/42tAdW4VjqQJ9QCPLriTTo=";
            };

            buildAttrs = {
              preBuild = ''
                patchShebangs $bazelOut/external/rules_haskell/haskell/private/ghc_wrapper.sh
                rm .bazelrc.nixpkgs
              '';
              installPhase = ''
                install -D -t $out/bin bazel-bin/replay
              '';
            };
          };

          defaultApp = flake-utils.lib.mkApp { drv = defaultPackage; };
          defaultPackage = packages.replay;

          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                nix-linter.enable = true;
                stylish-haskell.enable = true;
                buildifier = {
                  enable = true;
                  name = "buildifier";
                  entry = "${pkgs.buildifier}/bin/buildifier -mode=fix -lint=fix";
                  files = "^((WORKSPACE|BUILD)([.]bazel)?|.+[.]bzl)$";

                  # List of file types to run on (default: [ "file" ] (all files))
                  # see also https://pre-commit.com/#filtering-files-with-types
                  # You probably only need to specify one of `files` or `types`:
                  #types = [ "text" "c" ];
                };
              };
            };
          };

          devShell = pkgs.mkShell {
            shellHook = ''
              ${checks.pre-commit-check.shellHook}
            '';
            nativeBuildInputs = nativeBuildInputs ++ devTools;
          };
        }
      );
}
