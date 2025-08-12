load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")
load(
    "@rules_nixpkgs_go//:go.bzl",
    "nixpkgs_go_configure",
)
load("@rules_nixpkgs_python//:python.bzl", "nixpkgs_python_configure")

_nix_toolchains = tag_class(
    attrs = {"repository": attr.label()},
)

def _init_impl(mctx):
    for mod in mctx.modules:
        for nix_toolchains in mod.tags.nix_toolchains:
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
            break

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
    },
)
