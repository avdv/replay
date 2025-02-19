def _empty_repo_impl(rctx):
    rctx.file("BUILD.bazel", executable = False)

empty_repo = repository_rule(
    implementation = _empty_repo_impl,
)
