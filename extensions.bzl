load("@rules_nixpkgs_python//:python.bzl", "nixpkgs_python_configure")

def _nix_python_impl(mctx):
    for module in mctx.modules:
        for nixpkgs_tag in module.tags.nixpkgs:
            nixpkgs_python_configure(repository = nixpkgs_tag.repository, register = False)

_nixpkgs = tag_class(
    attrs = {
        "repository": attr.label(),
    },
)

nix_python_config = module_extension(
    implementation = _nix_python_impl,
    tag_classes = {
        "nixpkgs": _nixpkgs,
    },
)
