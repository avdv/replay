# Give your project a name. :)
workspace(name = "replay")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    sha256 = "b4e2c00da9bc6668fa0404275fecfdb31beb700abdba0e029e74cacc388d94d6",
    strip_prefix = "rules_haskell-0.13",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.13.tar.gz"],
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
    "nixpkgs_git_repository",
    "nixpkgs_python_configure",
)

nixpkgs_python_configure(
    repository = "@nixpkgs",
)

# Fetch a version of nixpkgs from GitHub.
# For more information see the documentation of rules_nixpkgs at
# https://github.com/tweag/rules_nixpkgs/blob/master/README.md
nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "20.09",
    # sha256 = …
)

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

# Fetch a GHC binary distribution from nixpkgs and register it as a toolchain.
# For more information:
# https://api.haskell.build/haskell/nixpkgs.html#haskell_register_ghc_nixpkgs
haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc884",
    repository = "@nixpkgs",
    version = "8.8.4",
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
    snapshot = "lts-16.31",
    # Note: `bazel run @stackage-unpinned//:pin` to update
    stack_snapshot_json = "//:stackage_snapshot.json",
)
