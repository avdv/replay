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
    flake-utils.lib.eachSystem [ "aarch64-linux" "x86_64-darwin" "x86_64-linux" ]
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          # pkgs = nixpkgs.legacyPackages.${system};
          nativeBuildInputs = with pkgs; [
            git
            haskell.compiler.ghc8107
            #bazel_4 #bazel-watcher bazel-buildtools
            openjdk11_headless #stylish-haskell
            haskell-language-server
          ] ++ lib.optional (system != "asdf") bazel-watcher;
        in
        rec {
          packages.replay = pkgs.buildBazelPackage {
            name = "replay-0.2.0";
            bazel = pkgs.bazel_4;
            src = ./.;

            removeRulesCC = false;

            bazelBuildFlags = [
              "--compilation_mode=opt" # optimize
              "--sandbox_debug"
              "--verbose_failures"
            ];

            bazelTarget = "//:replay";

            inherit nativeBuildInputs;

            fetchAttrs = {
              preBuild = ''ls -l'';
              sha256 = "sha256-v1vEFO0IGoDGyFSvsxQjs4vgmpAfDsiARbtjtkIyxY4=";
            };

            buildAttrs = {
              preBuild = ''
                ls -lh bazel-out/host/bin/external/rules_haskell/haskell/ghc_wrapper || true
                cat bazel-out/host/bin/external/rules_haskell/haskell/ghc_wrapper || true
              '';
              installPhase = ''
                install -Dm755 bazel-bin/replay $out/bin/replay
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
            nativeBuildInputs = nativeBuildInputs ++ [ pkgs.bazel_4 ];
          };
        }
      );
}
