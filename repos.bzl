load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")

_nix_cc = tag_class(
    attrs = {"repository": attr.label()},
)

def _init_impl(mctx):
    for mod in mctx.modules:
        for nix_cc in mod.tags.nix_cc:
            print("calling nixpkgs_cc_configure", nix_cc.repository)
            nixpkgs_cc_configure(
                name = "nixpkgs_cc",
                repository = nix_cc.repository,
                register = False,
            )
            break

init = module_extension(
    implementation = _init_impl,
    tag_classes = {
        "nix_cc": _nix_cc,
    },
)
