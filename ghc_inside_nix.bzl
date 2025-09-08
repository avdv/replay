"""Workspace rules for GHC inside nix-build"""

load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")

def _configure_python3_toolchain_impl(repository_ctx):
    cpu = get_cpu_value(repository_ctx)
    python3_path = repository_ctx.which("python")
    repository_ctx.file("BUILD.bazel", executable = False, content = """
load(
    "@bazel_tools//tools/python:toolchain.bzl",
    "py_runtime_pair",
)
py_runtime(
    name = "python3_runtime",
    interpreter_path = "{python3}",
    python_version = "PY3",
)
py_runtime_pair(
    name = "py_runtime_pair",
    py3_runtime = ":python3_runtime",
)
toolchain(
    name = "toolchain",
    toolchain = ":py_runtime_pair",
    toolchain_type = "@bazel_tools//tools/python:toolchain_type",
    exec_compatible_with = [
        "@platforms//cpu:{arch}",
        "@platforms//os:{os}",
    ],
    target_compatible_with = [
        "@platforms//cpu:{arch}",
        "@platforms//os:{os}",
    ],
)
""".format(
        python3 = python3_path,
        os = {
            "darwin": "osx",
            "darwin_x86_64": "osx",
            "darwin_arm64": "osx",
            "x64_windows": "windows",
        }.get(cpu, "linux"),
        arch = {
            "darwin_arm64": "arm64",
            "aarch64": "arm64",
        }.get(cpu, "x86_64"),
    ))

_config_python3_toolchain = repository_rule(
    _configure_python3_toolchain_impl,
    configure = True,
    local = True,
    environ = ["PATH"],
)

def _configure_python3_toolchain(name):
    """Autoconfigure python3 toolchain for GHC bindist
    `rules_haskell` requires Python 3 to build Haskell targets. Under Nix we
    use `rules_nixpkgs`'s `nixpkgs_python_configure` repository rule to use a
    nixpkgs provisioned Python toolchain. However, outside of Nix we have to
    rely on whatever Python toolchain is installed on the system.
    Bazel provides `@bazel_tools//tools/python:autodetecting_toolchain` for
    this purpose. However, in its current form, that toolchain does not
    support Python toolchains installed outside of standard system paths
    such as `/usr/bin:/bin:/usr/sbin`. The reason is that the toolchain does
    not look for a Python interpreter in a repository rule. Instead it uses
    wrapper scripts that look for a Python interpreter in `$PATH` within the
    sandboxed build actions.
    On MacOS, which, at the time of writing, only includes Python 2.7, users
    will want to install Python 3 in a non-system path, e.g. via homebrew or
    py_env. The auto detecting toolchain will not find this interpreter and
    builds will fail with the following error:
    ```
    Error occurred while attempting to use the default Python toolchain (@rules_python//python:autodetecting_toolchain).
    According to '/usr/bin/python -V', version is 'Python 2.7.10', but we need version 3. PATH is:
    /usr/bin:/bin:/usr/sbin
    Please ensure an interpreter with version 3 is available on this platform as 'python3' or 'python', or else register an appropriate Python toolchain as per the documentation for py_runtime_pair (https://github.com/bazelbuild/rules_python/blob/master/docs/python.md#py_runtime_pair).
    Note that prior to Bazel 0.27, there was no check to ensure that the interpreter's version matched the version declared by the target (#4815). If your build worked prior to Bazel 0.27, and you're sure your targets do not require Python 3, you can opt out of this version check by using the non-strict autodetecting toolchain instead of the standard autodetecting toolchain. This can be done by passing the flag `--extra_toolchains=@rules_python//python:autodetecting_toolchain_nonstrict` on the command line or adding it to your bazelrc.
    ```
    This function defins a custom auto detcting Python toolchain that looks for
    a Python 3 interpreter within a repository rule, so that Bazel's sandboxing
    does not restrict the visible installation paths. It then registers an
    appropriate Python toolchain, so that build actions themselves can still be
    sandboxed.
    """
    _config_python3_toolchain(name = name)

def _ghc_nixpkgs_haskell_toolchain_impl(repository_ctx):
    paths = {"@rules_haskell//haskell:private/pkgdb_to_bzl.py": repository_ctx.path(Label("//:pkgdb_to_bzl.py"))}

    compiler_flags_select = "select({})".format(
        repository_ctx.attr.compiler_flags_select or {
            "//conditions:default": [],
        },
    )
    nixpkgs_ghc_path = repository_ctx.which("ghc").dirname.dirname
    print("using ghc", nixpkgs_ghc_path)
    #repository_ctx.path(repository_ctx.attr.nixpkgs_ghc).dirname.dirname

    # Symlink content of ghc external repo. In effect, this repo has
    # the same content, but with a BUILD file that includes generated
    # content (not a static one like nixpkgs_package supports).
    for target in _find_children(repository_ctx, nixpkgs_ghc_path):
        basename = target.rpartition("/")[-1]
        print("symlink", target, "to", basename)
        repository_ctx.symlink(target, basename)

    ghc_name = "ghc-{}".format(repository_ctx.attr.version)
    # TODO check_ghc_version(repository_ctx)

    toolchain_libraries = _pkgdb_to_bzl(repository_ctx, paths, "lib/{}".format(ghc_name))["file_content"]
    locale_archive = repository_ctx.attr.locale_archive
    libdir_path = execute_or_fail_loudly(repository_ctx, ["bin/ghc", "--print-libdir"]).stdout.strip()
    docdir_path = execute_or_fail_loudly(repository_ctx, ["bin/ghc-pkg", "field", "base", "haddock-html", "--simple-output"]).stdout.strip()
    toolchain = define_rule(
        "haskell_toolchain",
        name = "toolchain-impl",
        libraries = "toolchain_libraries",
        # See Note [GHC toolchain files] in haskell/ghc_bindist.bzl
        libdir_path = repr(libdir_path),
        docdir_path = repr(docdir_path),
        tools = ["//:bin"],
        version = repr(repository_ctx.attr.version),
        static_runtime = repository_ctx.attr.static_runtime,
        fully_static_link = repository_ctx.attr.fully_static_link,
        ghcopts = "{} + {}".format(
            repository_ctx.attr.ghcopts,
            compiler_flags_select,
        ),
        haddock_flags = repository_ctx.attr.haddock_flags,
        repl_ghci_args = repository_ctx.attr.repl_ghci_args,
        cabalopts = repository_ctx.attr.cabalopts,
        locale_archive = repr(locale_archive) if locale_archive else None,
        locale = repr(repository_ctx.attr.locale),
    )
    repository_ctx.file(
        "BUILD",
        executable = False,
        content = """
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_import",
    "haskell_toolchain",
)
package(default_visibility = ["//visibility:public"])
filegroup(
    name = "bin",
    srcs = glob(["bin/*"]),
)
{toolchain_libraries}
{toolchain}
        """.format(
            toolchain_libraries = toolchain_libraries,
            toolchain = toolchain,
        ),
    )

_ghc_nixpkgs_haskell_toolchain = repository_rule(
    _ghc_nixpkgs_haskell_toolchain_impl,
    attrs = {
        # These attributes just forward to haskell_toolchain.
        # They are documented there.
        "version": attr.string(),
        "static_runtime": attr.bool(),
        "fully_static_link": attr.bool(),
        "ghcopts": attr.string_list(),
        "compiler_flags_select": attr.string_list_dict(),
        "haddock_flags": attr.string_list(),
        "cabalopts": attr.string_list(),
        "repl_ghci_args": attr.string_list(),
        "locale_archive": attr.string(),
        "nixpkgs_ghc_repo_name": attr.string(),
        # Unfortunately, repositories cannot depend on each other
        # directly. They can only depend on files inside each
        # repository. We need to be careful to depend on files that
        # change anytime any content in a repository changes, like
        # bin/ghc, which embeds the output path, which itself changes
        # if any input to the derivation changed.
        "nixpkgs_ghc": attr.label(),
        "locale": attr.string(
            default = "C.UTF-8",
        ),
    },
    configure = True,
    local = True,
)

# copied from rules_haskel/haskell/private/pkgdb_to_bzl.bzl

def _pkgdb_to_bzl(repository_ctx, paths, libdir):
    """Generate a BUILD file from a package database.

    Also creates symlinks to any resources stored outside of the GHC
    base directory.

    Args:
      repository_ctx: repository context.
      paths: a dictionary of label names to paths.
      pkgdir: the relative location of GHC's libdir
    """
    result = repository_ctx.execute([
        repository_ctx.which("python"),  # find_python(repository_ctx),
        paths["@rules_haskell//haskell:private/pkgdb_to_bzl.py"],
        repository_ctx.attr.name,
        libdir,
    ])
    if result.return_code:
        fail("Error executing pkgdb_to_bzl.py: {stderr}".format(stderr = result.stderr))

    result_dict = json.decode(result.stdout)

    # Haddock files on nixpkgs are stored outside of the ghc package
    # The pkgdb_to_bzl.py program generates bazel labels for theses files
    # and asks the parent process to generate the associated bazel symlink
    for line in result_dict["file_content"].split("\n"):
        if line.startswith("#SYMLINK:"):
            _, path, name = line.split(" ")
            repository_ctx.symlink(path, name)

    return result_dict

def register_ghc_in_nix(
        version,
        name = "rules_haskell_nix",
        is_static = None,  # DEPRECATED. See _check_static_attributes_compatibility.
        static_runtime = None,
        fully_static_link = None,
        build_file = None,
        build_file_content = None,
        compiler_flags = None,
        ghcopts = None,
        compiler_flags_select = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale_archive = None,
        attribute_path = "haskellPackages.ghc",
        sh_posix_attributes = None,
        nix_file = None,
        nix_file_deps = [],
        nixopts = None,
        locale = None,
        repositories = {},
        repository = None,
        nix_file_content = None,
        exec_constraints = None,
        target_constraints = None):
    # 1. add toolchain
    # 2.
    haskell_toolchain_repo_name = "{}_ghc_in_nix_haskell_toolchain".format(name)
    toolchain_repo_name = "{}_ghc_in_nix_toolchain".format(name)

    print(toolchain_repo_name)
    _ghc_nixpkgs_haskell_toolchain(
        name = haskell_toolchain_repo_name,
        version = version,
        static_runtime = static_runtime,
        fully_static_link = fully_static_link,
        ghcopts = ghcopts,
        compiler_flags_select = compiler_flags_select,
        haddock_flags = haddock_flags,
        cabalopts = cabalopts,
        repl_ghci_args = repl_ghci_args,
        locale_archive = locale_archive,
        locale = locale,
    )

    # toolchain definition.
    if (exec_constraints == None) != (target_constraints == None):
        fail("Both exec_constraints and target_constraints need to be provided or none of them.")
    _ghc_nixpkgs_toolchain(
        name = toolchain_repo_name,
        exec_constraints = exec_constraints,
        target_constraints = target_constraints,
        haskell_toolchain_repo_name = haskell_toolchain_repo_name,
    )

    local_python_repo_name = "rules_haskell_python_local"
    if local_python_repo_name not in native.existing_rules():
        _configure_python3_toolchain(name = local_python_repo_name)

    # Unix tools toolchain required for Cabal packages
    # sh_posix_nixpkgs_kwargs = dict(
    #     nix_file_deps = nix_file_deps,
    #     nixopts = nixopts,
    #     repositories = repositories,
    #     repository = repository,
    # )
    # if sh_posix_attributes != None:
    #     sh_posix_nixpkgs_kwargs["packages"] = sh_posix_attributes
    # nixpkgs_sh_posix_configure(
    #     name = nixpkgs_sh_posix_repo_name,
    #     **sh_posix_nixpkgs_kwargs
    # )

def _find_children(repository_ctx, target_dir):
    find_args = [
        "find",
        "-L",
        target_dir,
        "-maxdepth",
        "1",
        # otherwise the directory is printed as well
        "-mindepth",
        "1",
        # filenames can contain \n
        "-print0",
    ]
    exec_result = repository_ctx.execute(find_args)
    if exec_result.return_code:
        fail("_find_children() failed.")
    return exec_result.stdout.rstrip("\0").split("\0")

def execute_or_fail_loudly(
        repository_ctx,
        arguments,
        environment = {},
        working_directory = ""):
    """Execute the given command
    Fails if the command does not exit with exit-code 0.
    Args:
      arguments: List, the command line to execute.
    Returns:
      exec_result: The output of the command.
    """
    exec_result = repository_ctx.execute(
        arguments,
        environment = environment,
        quiet = True,
        working_directory = working_directory,
    )
    if exec_result.return_code != 0:
        arguments = [_as_string(x) for x in arguments]
        fail("\n".join(["Command failed: " + " ".join(arguments), exec_result.stderr]))
    return exec_result

def _as_string(v):
    if type(v) == "string":
        return v
    else:
        return repr(v)

def define_rule(rule_type, name, **kwargs):
    """Generate a string representing a rule definition.
    Take care to escape string values using repr().
    ### Examples
      ```bzl
      define_rule("myrule",
          name = "foo",
          myattr1 = repr("bar"),
          myattr2 = ["baz"],
      )
      ```
    """
    attrs = ["{} = {},".format(k, v) for k, v in kwargs.items() if v != None]
    skeleton = """\
{rule_type}(
    name = {name},
    {attrs}
)
"""
    return skeleton.format(
        rule_type = rule_type,
        name = repr(name),
        attrs = "\n    ".join(attrs),
    )

def _ghc_nixpkgs_toolchain_impl(repository_ctx):
    # These constraints might look tautological, because they always
    # match the host platform if it is the same as the target
    # platform. But they are important to state because Bazel
    # toolchain resolution prefers other toolchains with more specific
    # constraints otherwise.
    if repository_ctx.attr.target_constraints == [] and repository_ctx.attr.exec_constraints == []:
        arch = repository_ctx.os.arch
        os = repository_ctx.os.name
        if arch in ["x86_64", "amd64", "x64"]:
            target_constraints = ["@platforms//cpu:x86_64"]
        elif arch == "aarch64":
            target_constraints = ["@platforms//cpu:aarch64"]
        else:
            fail("unknown arch: " + arch)
        if os == "linux":
            target_constraints.append("@platforms//os:linux")
        elif os.startswith("mac os"):
            target_constraints.append("@platforms//os:macos")
        else:
            fail("unknown os: " + os)
        exec_constraints = list(target_constraints)
    else:
        target_constraints = repository_ctx.attr.target_constraints
        exec_constraints = list(repository_ctx.attr.exec_constraints)

    #exec_constraints.append("@io_tweag_rules_nixpkgs//nixpkgs/constraints:support_nix")

    repository_ctx.file(
        "BUILD",
        executable = False,
        content = """
toolchain(
    name = "toolchain",
    toolchain_type = "@rules_haskell//haskell:toolchain",
    toolchain = "@{haskell_toolchain}//:toolchain-impl",
    exec_compatible_with = {exec_constraints},
    target_compatible_with = {target_constraints},
)
        """.format(
            exec_constraints = exec_constraints,
            target_constraints = target_constraints,
            haskell_toolchain = repository_ctx.attr.haskell_toolchain_repo_name,
        ),
    )

_ghc_nixpkgs_toolchain = repository_rule(
    implementation = _ghc_nixpkgs_toolchain_impl,
    attrs = {
        "exec_constraints": attr.string_list(),
        "target_constraints": attr.string_list(),
        "haskell_toolchain_repo_name": attr.string(),
    },
    configure = True,
    local = True,
)
