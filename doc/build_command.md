<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack build` command and its synonyms

~~~text
stack build [TARGET] [--dry-run] [--pedantic] [--fast] [--ghc-options OPTIONS]
            [--flag PACKAGE:[-]FLAG] [--dependencies-only | --only-snapshot |
              --only-dependencies | --only-locals] [--file-watch |
              --file-watch-poll] [--watch-all] [--exec COMMAND [ARGUMENT(S)]]
            [--only-configure] [--trace] [--profile] [--no-strip]
            [--[no-]library-profiling] [--[no-]executable-profiling]
            [--[no-]library-stripping] [--[no-]executable-stripping]
            [--[no-]haddock] [--haddock-arguments HADDOCK_ARGS]
            [--[no-]open] [--[no-]haddock-deps] [--[no-]haddock-internal]
            [--[no-]haddock-hyperlink-source] [--[no-]copy-bins]
            [--[no-]copy-compiler-tool] [--[no-]prefetch] [--[no-]keep-going]
            [--[no-]keep-tmp-files] [--[no-]force-dirty] [--[no-]test]
            [--[no-]rerun-tests] [--ta|--test-arguments TEST_ARGS] [--coverage]
            [--no-run-tests] [--test-suite-timeout ARG]
            [--[no-]tests-allow-stdin] [--[no-]bench]
            [--ba|--benchmark-arguments BENCH_ARGS] [--no-run-benchmarks]
            [--[no-]reconfigure] [--cabal-verbosity VERBOSITY |
              --[no-]cabal-verbose] [--[no-]split-objs] [--skip ARG]
            [--[no-]interleaved-output] [--ddump-dir ARG]
~~~

`stack build` and its synonyms (`stack test`, `stack bench`, `stack haddock` and
`stack install`) are Stack's primany command. The command provides a simple
interface for simple tasks and flexibility for more complicated goals.

See the introductory part of Stack's
[user's guide](GUIDE.md#the-stack-build-command) for an introduction to the
command.

## Synonyms

The synonym commands for `stack build` are:

|Synonym command|Equivalent `stack build` command flag|
|---------------|-------------------------------------|
|`stack test`   |`stack build --test`                 |
|`stack bench`  |`stack build --bench`                |
|`stack haddock`|`stack build --haddock`              |
|`stack install`|`stack build --copy-bins`            |

The advantage of the synonym commands is that they are convenient and short. The
advantage of the flags is that they compose. See the examples below.

## Components

Every Cabal package is made up of one or more components. It can have an
optional library component, one or more optional executable components, one or
more optional test suite components, and one or more optional benchmark
components.

Stack allows you to identify a specific component to be built. For example,
`stack build mypackage:test:mytests` will build (and run - see further below)
the `mytests` component of the `mypackage` package. `mytests` must be a test
suite component.

By default, if a test suite component is targeted, the component is built and
run. The running behaviour can be disabled with the `--no-run-tests` flag.
Similarly, if a benchmark component is targeted, it is built and run unless the
running behaviour is disabled with the `--no-run-benchmarks` flag.

This ability to specify a component applies only to a local package. With
dependencies, Stack will *always* build the library (if present) and all
executables (if any), and ignore test suites and benchmarks. If you want more
control over a package, you must add it to your `packages` setting in your
project-level configuration file (`stack.yaml`).

## Target syntax

`stack build` takes a list of one or more optional *targets* to be built. The
supported syntaxes for targets are:

*   *package*, e.g. `stack build foobar`, is the most commonly used target. It
    will try to find the package in the following locations: local packages,
    extra deps, snapshots, and package index (e.g. Hackage). If it's found in
    the package index, then the latest version of that package from the index is
    implicitly added to your extra dependencies.

    If the package is a local package, the library and executable components are
    selected to be built. If the `--test` and `--bench` flags are set, then all
    of the test suite and benchmark components, respectively, are selected to be
    built.

*   *package identifier*, e.g. `stack build foobar-1.2.3`, is usually used to
    include specific package versions from the package index. If the version
    selected conflicts with an existing local package or extra dep, then Stack
    fails with an error. Otherwise, this is the same as using
    `stack build foobar`, except instead of using the latest version from the
    package index, the version specified is used.

*   *component*. Instead of referring to an entire package and letting Stack
    decide which components to build, you select individual components from
    inside a package. This can be done for more fine-grained control over which
    test suites to run, or to have a faster compilation cycle. There are
    multiple ways to refer to a specific component (provided for convenience):

    *   `packagename:comptype:compname` is the most explicit. The available
        comptypes are `exe`, `test`, and `bench`.

        !!! note

            When any `exe` component is specified, all of the package's
            executable components will be built. This is due to limitations in
            all currently released versions of Cabal. See
            [issue#1046](https://github.com/commercialhaskell/stack/issues/1406)

    *   `packagename:compname` allows you to leave out the component type, as
         that will (almost?) always be redundant with the component name. For
         example, `stack build mypackage:mytestsuite`.

    *   `:compname` is a useful shortcut, saying "find the component in all of
        the local packages." This will result in an error if multiple packages
        have a component with the same name. To continue the above example,
        `stack build :mytestsuite`.

*   *directory*, e.g. `stack build foo/bar`, will find all local packages that
    exist in the given directory hierarchy and then follow the same procedure as
    passing in package names as mentioned above. There's an important caveat
    here: if your directory name is parsed as one of the above target types, it
    will be treated as that. Explicitly starting your target with `./` can be a
    good way to avoid that, e.g. `stack build ./foo`.

    !!! note

        `stack build .` will target local packages in the current working
        directory or its subdirectories.

`stack build` with no targets specified will build all local packages.

Command `stack ide targets` to get a list of the available targets in your
project.

## Controlling what gets built

Stack will automatically build the necessary dependencies. See the introductory
part of Stack's [user's guide](GUIDE.md#the-stack-build-command) for information
about how these dependencies get specified.

In addition to specifying targets, you can also control what gets built, or
retained, with the following flags:

### The `stack build --bench` flag

Pass the flag to add benchmark components to the targets, if specific components
are not identified.

### The `stack build --dependencies-only` flag

Pass the flag to skip building the targets. The flag `--only-dependencies` has
the same effect.

### The `stack build --[no-]dry-run` flag

Default: Disabled

Set the flag to build nothing and output information about the build plan.

### The `stack build --flag` option

`stack build --flag <package_name>:[-]<flag_name>` sets (or unsets) the
specified Cabal flag for the specified package.

This option can be specified multiple times to set (or unset) multiple Cabal
flags.

The same Cabal flag name can be set (or unset) for multiple packages (at the
command line only) with:

~~~text
stack build --flag *:[-]<flag)name>
~~~

!!! note

    Currently you needs to list all of your modules that interpret flags in the
    `other-modules` section of a Cabal file. Cabal (the tool) has a different
    behavior currently and doesn't require that the modules be listed. This may
    change in a future release.

### The `stack build --[no-]force-dirty` flag

Default: Disabled

Set the flag to force rebuild of packages even when it doesn't seem necessary
based on file dirtiness.

### The `stack build --[no-]haddock` flag

Default: Disabled

Set the flag to build Haddock documentation. This may cause a lot of packages to
get re-built, so that the documentation links work.

### The `stack build --[no-]keep-going` flag

Default (`stack build`): Disabled

Default (`stack test` or `stack bench`): Enabled

Set the flag to continue building packages even after some build step fails.
The packages which depend upon the failed build won't get built.

### The `stack build --[no-]keep-tmp-files` flag

Default: Disabled

Set the flag to keep intermediate files and build directories that would
otherwise be considered temporary and deleted. It may be useful to inspect
these, if a build fails. By default, they are not kept.

### The `stack build --only-configure` flag

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

Pass the flag to perform only the configure step, not any builds. This is
intended for tool usage. It may break when used on multiple packages at once.

!!! note

    If there are downstream actions that require a package to be built then a
    full build will occur, even if the flag is passed.

### The `stack build --only-dependencies` flag

Pass the flag to skip building the targets. The flag `--dependencies-only` has
the same effect.

### The `stack build --only-snapshot` flag

Pass the flag to build only snapshot dependencies, which are cached and shared
with other projects.

### The `stack build --[no-]reconfigure` flag

Default: Disabled

Set the flag to force reconfiguration even when it doesn't seem necessary based
on file dirtiness. This is sometimes useful with custom `Setup.hs` files, in
particular when they depend on external data files.

### The `stack build --skip` option

`stack build --skip <component>` skips building the specified components of a
local package. It allows you to skip test suites and benchmark without
specifying other components (e.g. `stack test --skip long-test-suite` will run
the tests without the `long-test-suite` test suite). Be aware that skipping
executables won't work the first time the package is built due to an issue in
[Cabal](https://github.com/commercialhaskell/stack/issues/3229).

This option can be specified multiple times to skip multiple components.

### The `stack build --test` flag

Pass the flag to add test suite components to the targets, if specific
components are not identified.

## Other flags and options

There are a number of other flags accepted by `stack build`. Instead of listing
all of them, please use `stack build --help`. Some particularly convenient ones
worth mentioning here since they compose well with the rest of the build system
as described:

### The `stack build --coverage` flag

Pass the flag to generate a code coverage report. For further information, see
the [code coverage](hpc_command.md) documentation.

### The `stack build --exec` option

`stack build --exec "<command> [<arguments>]"` will run a command after a
successful build.

### The `stack build --file-watch` flag

Pass the flag to rebuild your project every time a file changes. By default it
will take into account all files belonging to the targets you specify. See also
the `--watch-all` flag.

### The `stack build --[no-]interleaved-output` flag

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Default: Enabled

Set the flag for interleaved output. With interleaved output, each line of
output from each package being built (targets and dependencies) is sent to the
console as it happens and output relating to different packages can be
interleaved. Each line will be prefixed with the name of the relevant package.
The spacing between the prefix and the output will be set based on the longest
relevant package name, so that the start of the output itself aligns. For
example (extract):

~~~text
hpack            > build
mustache         > configure
hpack            > Preprocessing library for hpack-0.35.0..
hpack            > Building library for hpack-0.35.0..
mustache         > Configuring mustache-2.4.1...
hpack            > [ 1 of 29] Compiling Data.Aeson.Config.Key
hpack            > [ 2 of 29] Compiling Data.Aeson.Config.KeyMap
mustache         > build
hpack            > [ 3 of 29] Compiling Data.Aeson.Config.Util
mustache         > Preprocessing library for mustache-2.4.1..
mustache         > Building library for mustache-2.4.1..
hpack            > [ 4 of 29] Compiling Hpack.Haskell
hpack            > [ 5 of 29] Compiling Hpack.Utf8
mustache         > [1 of 8] Compiling Paths_mustache
hpack            > [ 6 of 29] Compiling Imports
hpack            > [ 7 of 29] Compiling Hpack.Util
mustache         > [2 of 8] Compiling Text.Mustache.Internal
~~~

Unset the flag for non-interleaved output. With non-interleaved output, the
build output from GHC (as opposed to from Stack) in respect of dependencies is
ignored. The behaviour then depends whether there is one target package or more
than one. There can be one target if the project has a single package or if one
package is targetted in a multi-package project (for example, using
`stack build <package_name>`).

* **One target package:** The build output for the target package is sent to the
  console as it happens.

* **More than one target package:** The build output from GHC (as opposed to
  from Stack) for each target package is sent to a log file for that package,
  unless an error occurs. At the end of the build, the location of the directory
  containing the log files is reported. To also output the contents of the log
  files to the console at the end of the build, use Stack's `dump-logs` option.
  For further information about that option, see the
  [YAML configuration](yaml_configuration.md#dump-logs) documentation. The
  default `dump-logs` mode is to output the contents of the log files that are
  warnings.

### The `stack build --pedantic` flag

Pass the flag to build your project with the GHC options `-Wall` and `-Werror`.
`-Wall` turns on all warning options that indicate potentially suspicious code.
`-Werror` makes any warning into a fatal error.

### The `stack build --watch-all` flag

Pass the flag to rebuild your project every time any local file changes (from
project packages or from local dependencies). See also the `--file-watch` flag.

### The `stack build --tests-allow-stdin` flag

[:octicons-tag-24: 2.9.3](https://github.com/commercialhaskell/stack/releases/tag/v2.9.3)

Default: Enabled

Cabal defines a test suite interface
['exitcode-stdio-1.0'](https://hackage.haskell.org/package/Cabal-syntax-3.8.1.0/docs/Distribution-Types-TestSuiteInterface.html#v:TestSuiteExeV1.0)
where the test suite takes the form of an executable and the executable takes
nothing on the standard input channel (`stdin`). Pass this flag to override that
specification and allow the executable to receive input on that channel. If you
pass `--no-tests-allow-stdin` and the executable seeks input on the standard
input channel, an exception will be thown.

## Examples

*   `stack build --test --copy-bins` or, equivalently, `stack test --copy-bins`
    or `stack install --test`, will build libraries, executables, and test
    suites, run the test suites, and then copy the executables to Stack's local
    binary directory (see `stack path --local-bin`). This is an example of the
    flags composing.

*   The following example uses the
    `wai` [repository](https://github.com/yesodweb/wai/)). The `wai` project
    comprises a number of packages, including `wai-extra` and `warp`. The
    command:

    ~~~text
    stack build --file-watch --test --copy-bins --haddock wai-extra :warp warp:doctest --exec 'echo Yay, it worked!'
    ~~~

    will start Stack up in file watch mode, waiting for files in your project to
    change. When first starting, and each time a file changes, it will do all of
    the following.

    *   Build the `wai-extra` package and its test suites
    *   Build the `warp` executable
    *   Build the `warp` package's `doctest` component (which is a test site)
    *   Run all of the `wai-extra` package's test suite components and the
        `doctest` test suite component
    *   If all of that succeeds:
          * Copy generated executables to Stack's local binary directory (see
            `stack path --local-bin`)
          * Run the command `echo Yay, it worked!`
