{
  description = "replay";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    bazel-central-registry = {
      url = "github:bazelbuild/bazel-central-registry";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nix-filter, flake-utils, pre-commit-hooks, bazel-central-registry, ... }:
    with flake-utils.lib.system;
    flake-utils.lib.eachSystem [ aarch64-linux x86_64-darwin x86_64-linux ]
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          filter = nix-filter.lib;
          bazel = pkgs.bazel_6;
          ghcVersion = builtins.head (builtins.match ''.*[ \n]*GHC_VERSION *= *"([^ \n]+)".*'' (builtins.readFile ./ghc.bzl));
          ghc = import ./nix/ghc.nix
            {
              inherit ghcVersion pkgs;
            };
          ghcWithHoogle = import ./nix/ghc.nix
            {
              inherit ghcVersion pkgs;
              withHoogle = true;
            };
          nativeBuildInputs = with pkgs; [
            git
            installShellFiles
            python3
          ];
          devTools = let inherit (pkgs) bazel-watcher buildifier haskell-language-server lib; in
            [ bazel buildifier ghcWithHoogle haskell-language-server ] ++ lib.optional (!bazel-watcher.meta.broken) bazel-watcher;
        in
        rec {
          packages.replay = pkgs.buildBazelPackage {
            pname = "replay";
            version = with builtins; head (split "\n" (readFile ./VERSION));
            src = filter {
              root = self;
              exclude = [
                (filter.matchExt ".md")
                ./.bazel-nix.rc # disable the nixpkgs toolchain inside nix
                ./.envrc
                ./.github
                ./.hie-bios
                ./flake.nix
                ./flake.lock
                ./hie.yaml
                ./images
                ./shell.nix
              ];
            };

            removeRulesCC = false;

            bazelFlags = [
              "--extra_toolchains=@rules_haskell_nix_ghc_in_nix_toolchain//:toolchain"
              "--registry"
              "file://${bazel-central-registry}"
            ];

            bazelBuildFlags = [
              "--compilation_mode=opt" # optimize
              "--verbose_failures"
            ];

            passthru = {
              exePath = "/bin/replay";
            };

            inherit bazel;

            bazelTargets = [ "//:replay" ];

            nativeBuildInputs = nativeBuildInputs ++ [ ghc ];

            fetchAttrs = {
              sha256 = "sha256-Cj01DsVGODI2RRLQyChXqR73kOfMQ78yQr/pAk0wDdc=";
            };

            buildAttrs = {
              preBuild = ''
                patchShebangs $bazelOut/external/rules_haskell~0.19/haskell/private/ghc_wrapper.sh
              '';
              installPhase = ''
                install -D -t $out/bin bazel-bin/replay

                installShellCompletion --cmd replay \
                   --bash <( bazel-bin/replay --bash-completion-script $out/bin/replay ) \
                   --fish <( bazel-bin/replay --fish-completion-script $out/bin/replay ) \
                   --zsh <( bazel-bin/replay --zsh-completion-script $out/bin/replay )
              '';
            };
          };
          packages.default = packages.replay;

          apps.default = flake-utils.lib.mkApp { drv = defaultPackage; };

          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                shellcheck.enable = true;
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

          devShells.default = pkgs.mkShellNoCC {
            shellHook = ''
              ${checks.pre-commit-check.shellHook}
            '';
            inherit nativeBuildInputs;
            packages = devTools;
          };

          # compatibility for nix < 2.7.0
          defaultApp = apps.default;
          defaultPackage = packages.default;
          devShell = devShells.default;
        }
      );
}
