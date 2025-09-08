load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")
load(
    "@rules_nixpkgs_go//:go.bzl",
    "nixpkgs_go_configure",
)
load("@rules_nixpkgs_python//:python.bzl", "nixpkgs_python_configure")
load("//:ghc_inside_nix.bzl", "register_ghc_in_nix")

_nix_toolchains = tag_class(
    attrs = {"repository": attr.label()},
)

_ghc_in_nix = tag_class(
    attrs = {
        "ghc_opts": attr.string_list(),
        "version": attr.string(),
    }
)

def _handle_ghc_in_nix(mctx, attrs):
    register_ghc_in_nix(
        attribute_path = None,
        ghcopts = attrs.ghc_opts,
        nix_file = "//nix:ghc.nix",
        nix_file_deps = [
            "//:nixpkgs.nix",
            "//:flake.lock",
        ],
        nixopts = [
            "--argstr",
            "ghcVersion",
            attrs.version,
        ],
        version = attrs.version,
    )

def _init_impl(mctx):
    repository = None
    for mod in mctx.modules:
        for nix_toolchains in mod.tags.nix_toolchains:
            repository = nix_toolchains.repository
            break
        if repository:
            break
    if not repository:
        fail("using a nix_toolchains tag is mandatory")

    for mod in mctx.modules:
        for ghc_in_nix in mod.tags.ghc_in_nix:
           _handle_ghc_in_nix(mctx, ghc_in_nix)

    nixpkgs_cc_configure(
        name = "nixpkgs_cc",
        repository = nix_toolchains.repository,
        register = False,
    )
    nixpkgs_go_configure(
        repository = nix_toolchains.repository,
        rules_go_repo_name = "io_bazel_rules_go",
        register = False,
    )
    nixpkgs_python_configure(
        repository = nix_toolchains.repository,
        register = False,
    )

    http_archive(
        name = "io_tweag_gazelle_haskell_modules",
        sha256 = "6f763feca1a7526f30177a7a685b9c24beee5ca6df9c47358e885b02e50ffe61",
        strip_prefix = "gazelle_haskell_modules-f28c6c7c1f2e154dbeb67956f97220e4359a6b59",
        url = "https://github.com/tweag/gazelle_haskell_modules/archive/f28c6c7c1f2e154dbeb67956f97220e4359a6b59.tar.gz",
    )            

init = module_extension(
    implementation = _init_impl,
    tag_classes = {
        "nix_toolchains": _nix_toolchains,
        "ghc_in_nix": _ghc_in_nix,
    },
)
