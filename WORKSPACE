# Give your project a name. :)
workspace(name = "replay")

# Load the repository rule to download a git repo
load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "git_repository",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
git_repository(
    name = "rules_haskell",
    commit = "d3d8bede5597c4ffc373f9742a6a733c728505b2",
    remote = "https://github.com/tweag/rules_haskell.git",
    shallow_since = "1637785596 +0000",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

# Load nixpkgs_git_repository from rules_nixpkgs,
# which was already initialized by rules_haskell_dependencies above.
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_python_configure",
)

nixpkgs_python_configure(
    repository = "@nixpkgs",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nix:nixpkgs.nix",
    nix_file_deps = [
        "//nix:BUILD",
        "//nix:default.nix",
        "//nix:sources.nix",
        "//nix:sources.json",
    ],
)

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

# Fetch a GHC binary distribution from nixpkgs and register it as a toolchain.
# For more information:
# https://api.haskell.build/haskell/nixpkgs.html#haskell_register_ghc_nixpkgs
haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc8107",
    repository = "@nixpkgs",
    version = "8.10.7",
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    extra_deps = {"zlib": ["@zlib.hs"]},
    flags = {
        # Sets the default explicitly to demonstrate the flags attribute.
        "zlib": [
            "-non-blocking-ffi",
            "-pkg-config",
        ],
    },
    packages = [
        "base",
        "brick",
        "bytestring",
        "hinotify",
        "hspec",
        "microlens",
        "microlens-th",
        "mtl",
        "optparse-applicative",
        "process",
        "text",
        "text-show",
        "unix",
        "vty",
    ],
    snapshot = "lts-18.18",
    # Note: `bazel run @stackage-unpinned//:pin` to update
    stack_snapshot_json = "//:stackage_snapshot.json",
)
