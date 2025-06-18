<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack ghci` and `stack repl` commands

~~~text
stack ghci [TARGET/FILE] [--pedantic] [--ghci-options OPTIONS]
           [--ghc-options OPTIONS] [--flag PACKAGE:[-]FLAG] [--with-ghc GHC]
           [--[no-]load] [--package PACKAGE] [--main-is TARGET]
           [--load-local-deps] [--[no-]package-hiding] [--only-main] [--trace]
           [--profile] [--no-strip] [--[no-]test] [--[no-]bench]
~~~

A read–evaluate–print loop (REPL) environment takes single user inputs, executes
them, and returns the result to the user. GHCi is GHC's interactive environment.
The `stack ghci` or `stack repl` commands, which are equivalent, allow you to
load components and files of your project into GHCi.

The command accepts the same TARGET syntax as
[`stack build`](build_command.md#target-syntax). By default:

* Stack loads up GHCi with all the library and executable components of all the
  packages in the project. Pass the flag `--test` to include test suite
  components (unlike `stack build`, test suites will not be run). Pass the flag
  `--bench` to include benchmark components (unlike `stack build`, benchmarks
  will not be run).

It is also possible to specify a module source code file. For example:

~~~text
stack ghci src/MyFile.hs
~~~

Stack will identify which component the file is associated with, and use the
options from that component.

Pass the `--package` option to load GHCi with an additional package that is not
a direct dependency of your components. This option can be specified multiple
times.

Pass the option `--flag <package_name>:<flag_name>` or
`--flag <package_name:-<flag_name>` to set or unset a Cabal flag. This option
can be specified multiple times. The same Cabal flag name can be set (or unset)
for multiple packages with:

~~~text
--flag *:[-]<flag_name>
~~~

!!! note

    In order to set a Cabal flag for a GHC boot package, the package must either
    be an extra-dep or the package version must be specified with the
    `--package` option.

By default:

*   Stack uses the GHC specified in Stack's configuration. Pass the `--with-ghc`
    option with a file path to the executable to specify a different GHC
    executable;

*   Stack performs an inital build step. Pass the `--no-build` flag to skip the
    step. Pass the `--ghc-options` option to pass flags or options to GHC. Pass
    the `--profile`, `--no-strip`, `--trace` flags for the same behaviour as in
    the case of the `stack build` command.

    !!! info

        Not performing the initial build step speeds up the startup of GHCi. It
        only works if the dependencies of the loaded packages have already been
        built.

*   Stack runs GHCi via `ghc --interactive`. Pass the `--ghc-options` option to
    pass flags or options to GHC (during the initial build step) and to GHCi.
    Pass the `--pedantic` flag to pass the GHC options `-Wall` and `-Werror` to
    GHCi (only). Pass the `--ghci-options` option to pass flags or options to
    GHCi (only).

*   Stack configures GHCi to hide unnecessary packages, unless no packages are
    targeted and no additional packages are specified. Pass the
    `--package-hiding` flag to hide unnecessary packages or
    `--no-package-hiding` flag not to hide unnecessary packages.

*   Stack loads and imports all of the modules for each target. Pass the
    `--no-load` flag to skip the loading of modules. Pass the `--only-main` flag
    to skip the loading of modules other than the main module. Pass the
    `--load-local-deps` flag to include all local dependencies of targets.

    !!! info

        Not loading modules speeds up the startup of GHCi. Once in GHCi, you can
        use `:load myModule` to load a specific module in your project.

    !!! info

        The `--only-main` flag can be useful if:

        1.  You're loading the project in order to run it in GHCi (e.g. via
            `main`), and you intend to reload while developing. Without flag,
            you will need to quit and restart GHCi whenever a module gets
            deleted. With the flag, reloading should work fine in this case.

        2.  If many of your modules have exports named the same thing, then
            you'll need to refer to them using qualified names. To avoid this,
            use the `--only-main` flag to start with a blank slate and just
            import the modules you are interested in.

*   If there are multiple definitions for the `Main` module, Stack will ask you
    to select one from a list of options. Pass the `--main-is <target>` option
    to specify which `Main` module to load.

Stack combines all of the GHC options of components.

!!! note

    Combining GHC options should work out when packages share similar
    conventions. However, conflicts may arise, such as when one component
    defines default extensions which aren't assumed by another. For example,
    specifying `NoImplicitPrelude` in one component but not another is likely to
    cause failures. GHCi will be run with `-XNoImplicitPrelude`, but it is
    likely that modules in the other component assume that the `Prelude` is
    implicitly imported.

`stack ghci` configures GHCi by using a GHCi script file. Such files are located
in subdirectories of `<XDG_CACHE_HOME>/stack/ghci-script`, where
`<XDG_CACHE_HOME>` refers to the
[XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
for user-specific non-essential (cached) data.

=== "Unix-like"

    The default for `<XDG_CACHE_HOME>` is `$HOME/.cache`.

=== "Windows"

     On Windows, the default for `<XDG_CACHE_HOME>` is `$Env:LOCALAPPDATA`.

=== "Windows (Command Prompt)"

     On Windows, the default for `<XDG_CACHE_HOME>` is `%LOCALAPPDATA%`.

## Running plain GHCi

`stack ghci` always runs GHCi configured to load code from packages in your
project. In particular, this means it passes in flags like `-hide-all-packages`
and `-package-id=` in order to configure which packages are visible to GHCi.

For doing experiments which just involve packages installed in your databases,
it may be useful to run GHCi plainly like:

~~~text
stack exec ghci
~~~

This will run a plain GHCi in an environment which includes `GHC_PACKAGE_PATH`,
and so will have access to your databases.

!!! note

    Running `stack ghci` on a pristine copy of the code doesn't currently build
    libraries
    (issue [#2790](https://github.com/commercialhaskell/stack/issues/2790)) or
    internal libraries
    (issue [#4148](https://github.com/commercialhaskell/stack/issues/4148)). It
    is recommended to always use `stack build` before using `stack ghci`, until
    these two issues are closed.
