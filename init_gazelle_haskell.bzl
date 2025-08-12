load("@io_tweag_gazelle_haskell_modules//:defs.bzl", "gazelle_haskell_modules_dependencies")

_init = tag_class(
    attrs = {"json": attr.label()},
)

def _gazelle_haskell_impl(mctx):
    for mod in mctx.modules:
        for init in mod.tags.init:
            gazelle_haskell_modules_dependencies(
                json = init.json,
            )

gazelle_haskell = module_extension(
    implementation = _gazelle_haskell_impl,
    tag_classes = {
        "init": _init,
    },
)
