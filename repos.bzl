load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")
load(
    "@rules_nixpkgs_go//:go.bzl",
    "nixpkgs_go_configure",
)

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
                #rules_go_repo_name = "io_bazel_rules_go",
                register = False,
            )
            break

init = module_extension(
    implementation = _init_impl,
    tag_classes = {
        "nix_toolchains": _nix_toolchains,
    },
)
