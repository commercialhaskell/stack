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
            [--[no-]haddock-hyperlink-source] [--[no-]haddock-for-hackage]
            [--[no-]copy-bins] [--[no-]copy-compiler-tool] [--[no-]prefetch]
            [--[no-]keep-going] [--[no-]keep-tmp-files] [--[no-]force-dirty]
            [--[no-]test] [--[no-]rerun-tests] [--ta|--test-arguments TEST_ARGS]
            [--coverage] [--no-run-tests] [--test-suite-timeout ARG]
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
optional public library component, one or more optional executable components,
one or more optional test suite components, and one or more optional benchmark
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

    If *package* is a GHC boot package (packages that come with GHC and are
    included in GHC's global package database), the behaviour can be complex.
    If the boot package has not been 'replaced', then `stack build` will,
    effectively, do nothing. However, if the boot package has been 'replaced'
    then `stack build` will specify the latest version of that package in the
    package index, which may differ from the version provided by the version of
    GHC specified by the snapshot. A boot package will be treated as 'replaced'
    if the package is included directly in the Stackage snapshot or it depends
    on a package included directly in the snapshot. Stackage snapshots do not
    include directly most boot packages but some snapshots may include directly
    some boot packages. In particular, some snapshots include directly `Win32`
    (which is a boot package on Windows) while others do not. For example, if
    `Cabal` (a boot package) is not a local package or an extra dep, then
    `stack build Cabal` with Stackage snapshot LTS Haskell 20.25 will:

    * on Windows, try to build the latest version of `Cabal` in the package
      index (because that snapshot includes `Win32` directly, and `Cabal`
      depends on `Win32` and so is treated as 'replaced'); and
    * on non-Windows, effectively, do nothing (because `Cabal` is not
      'replaced').

*   *package identifier*, e.g. `stack build foobar-1.2.3`, is usually used to
    include specific package versions from the package index.

    If the package name conflicts with that of a local package, then Stack
    fails with an error.

    Otherwise, this is the same as using `stack build foobar` (that is, ignoring
    the specified version), unless the specified version exists in the package
    index. If it exists, then the latest revision of that version from the
    package index is used.

*   *component*. Instead of referring to an entire package and letting Stack
    decide which components to build, you select individual components from
    inside a package. This can be done for more fine-grained control over which
    test suites to run, or to have a faster compilation cycle. There are
    multiple ways to refer to a specific component (provided for convenience):

    *   `<package-name>:lib` or `<package-name>:<comp-type>:<comp-name>` (where
        the component type, `<comp-type>`, is one of `exe`, `test`, or `bench`)
        is the most explicit. The library component type (`lib`) does not have
        an associated component name, `<comp-name>`.

        !!! note

            When any `exe` component is specified, all of the package's
            executable components will be built. This is due to limitations in
            all currently released versions of Cabal. See
            [issue#1046](https://github.com/commercialhaskell/stack/issues/1406)

    *   `<package-name>:<comp-name>` allows you to leave out the component type,
         as that will often be unique for a given component name. For
         example, `stack build mypackage:mytestsuite`.

    *   `:<comp-name>` is a useful shortcut, saying "find the component
        `<comp-name>` in all of the local packages". This will result in an
        error if more than one package has a component with the specified name.
        To continue the above example, `stack build :mytestsuite`.

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

For further information about available targets, see the
[`stack ide targets` command](ide_command.md).

## Controlling what gets built

Stack will automatically build the necessary dependencies. See the introductory
part of Stack's [user's guide](GUIDE.md#the-stack-build-command) for information
about how these dependencies get specified.

In addition to specifying targets, you can also control what gets built, or
retained, with the flags and options listed below. You can also affect what gets
built by specifying Cabal (the library) options for the configure step
of the Cabal build process (for further information, see the documentation for
the [configure-options](yaml_configuration.md#configure-options) configuration
option).

### `--bench` flag

Pass the flag to add benchmark components to the targets, if specific components
are not identified. The `stack bench` synonym sets this flag.

### `--dependencies-only` flag

Pass the flag to skip building the targets. The flag `--only-dependencies` has
the same effect.

### `--[no-]dry-run` flag

Default: Disabled

Set the flag to build nothing and output information about the build plan.

### `--flag` option

`stack build --flag <package_name>:[-]<flag_name>` sets (or unsets) the
specified Cabal flag for the specified package.

This option can be specified multiple times to set (or unset) multiple Cabal
flags.

The same Cabal flag name can be set (or unset) for multiple packages (at the
command line only) with:

~~~text
stack build --flag *:[-]<flag_name>
~~~

!!! note

    Currently you needs to list all of your modules that interpret flags in the
    `other-modules` section of a Cabal file. Cabal (the tool) has a different
    behavior currently and doesn't require that the modules be listed. This may
    change in a future release.

### `--[no-]force-dirty` flag

Default: Disabled

Set the flag to force rebuild of packages even when it doesn't seem necessary
based on file dirtiness.

### `--[no-]haddock` flag

Default: Disabled

Set the flag to build Haddock documentation. This may cause a lot of packages to
get re-built, so that the documentation links work. The `stack haddock` synonym
sets this flag.

Stack applies Haddock's `--gen-contents` and `--gen-index` flags to generate a
single HTML contents and index for multiple sets of Haddock documentation.

!!! warning

    On Windows, the values for the `haddock-interfaces` and `haddock-html` keys
    in the `*.conf` files for boot packages provided with certain versions of
    GHC (in its `lib\package.conf.d` directory) can be corrupt and refer to
    non-existent files and directories. For example, in the case of GHC 9.0.1
    to GHC 9.8.1 the references are to
    `${pkgroot}/../../docs/html/libraries/...` or
    `${pkgroot}/../../doc/html/libraries/...` instead of
    `${pkgroot}/../docs/html/libraries/...` or
    `${pkgroot}/../doc/html/libraries/...`. Until those values are corrected,
    Haddock documentation will be missing links to what those packages expose.

### `--haddock-arguments` option

`stack haddock --haddock-arguments <haddock_argument(s)>` passes the specified
arguments to the Haddock tool.

Specified arguments are separated by spaces. Arguments can be unquoted (if they
do not contain space or `"` characters) or quoted (`""`). Quoted arguments can
include 'escaped' characters, escaped with an initial `\` character.

!!! note

    Haddock's `--latex` flag is incompatible with the Haddock flags used by
    Stack to generate a single HTML contents and index.

### `--[no-]haddock-deps` flag

Default: Enabled (if building Haddock documnentation)

Unset the flag to disable building Haddock documentation for dependencies.

### `--[no-]haddock-for-haddock` flag

:octicons-beaker-24: Experimental

[:octicons-tag-24: 2.15.1](https://github.com/commercialhaskell/stack/releases/tag/v2.15.1)

Default: Disabled

Set the flag to build local packages with flags to generate Haddock
documentation suitable for upload to Hackage. The form of the Haddock
documentation generated for other packages is unaffected.

For each local package:

* the generated Haddock documentation files are in directory
  `doc\html\<package_version>-docs\`, relative to Stack's dist work directory
  (see [`stack path --dist-dir`](path_command.md)); and
* an archive of the `<package_version>-docs` directory and its contents is in
  Stack's dist work directory.

If the flag is set:

* the [`--[no-]haddock-hyperlink-source`](#-no-haddock-hyperlink-source-flag)
  flag is ignored and `--haddock-hyperlink-source` is implied;
* the [`--[no-]haddock-deps`](#-no-haddock-deps-flag) flag is ignored and the
  default value for the flag is implied;
* the [`--[no-]haddock-internal`](#-no-haddock-hyperlink-internal-flag) flag is
  ignored and `--no-haddock-internal` is implied;
* the [`--[no-]open`](#-no-open-flag) flag is ignored and `--no-open` is
  implied; and
* the [`--[no-]force-dirty`](#-no-force-dirty-flag) flag is ignored and
  `--force-dirty` is implied.

!!! info

    Stack does not distinguish the building of Haddock documentation for Hackage
    from the building of Haddock documentation generally, which is why the
    `--force-dirty` flag is implied.

!!! note

    If set, Haddock will warn that `-source-*` options are ignored when
    `--hyperlinked-source` is enabled. That is due to a known bug in Cabal
    (the libiary).

!!! note

    If set, Cabal (the library) will report that documentation has been created
    in `index.html` and `<package_name>.txt` files. Those files do not exist.
    That false report is due to a known bug in Cabal (the library).

### `--[no-]haddock-hyperlink-source` flag

Default: Enabled

Unset the flag to disable building building hyperlinked source for Haddock.

If the [`--haddock-for-hackage`](#-no-haddock-for-haddock-flag) flag is passed,
this flag is ignored.

### `--[no-]haddock-internal` flag

Default: Disabled

Set the flag to enable building Haddock documentation for internal modules.

If the [`--haddock-for-hackage`](#-no-haddock-for-haddock-flag) flag is passed,
this flag is ignored.

### `--[no-]keep-going` flag

Default (`stack build`): Disabled

Default (`stack test` or `stack bench`): Enabled

Set the flag to continue building packages even after some build step fails.
The packages which depend upon the failed build won't get built.

### `--[no-]keep-tmp-files` flag

Default: Disabled

Set the flag to keep intermediate files and build directories that would
otherwise be considered temporary and deleted. It may be useful to inspect
these, if a build fails. By default, they are not kept.

### `--only-configure` flag

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

Pass the flag to perform only the configure step, not any builds. This is
intended for tool usage. It may break when used on multiple packages at once.

!!! note

    If there are downstream actions that require a package to be built then a
    full build will occur, even if the flag is passed.

### `--only-dependencies` flag

Pass the flag to skip building the targets. The flag `--dependencies-only` has
the same effect.

### `--only-locals` flag

Pass the flag to build only packages in the local database. Fails if the build
plan includes packages in the snapshot database.

### `--only-snapshot` flag

Pass the flag to build only snapshot dependencies, which are cached and shared
with other projects.

### `--[no-]reconfigure` flag

Default: Disabled

Set the flag to force reconfiguration even when it doesn't seem necessary based
on file dirtiness. This is sometimes useful with custom `Setup.hs` files, in
particular when they depend on external data files.

### `--skip` option

`stack build --skip <component>` skips building the specified components of a
local package. It allows you to skip test suites and benchmark without
specifying other components (e.g. `stack test --skip long-test-suite` will run
the tests without the `long-test-suite` test suite). Be aware that skipping
executables won't work the first time the package is built due to an issue in
[Cabal](https://github.com/commercialhaskell/stack/issues/3229).

This option can be specified multiple times to skip multiple components.

### `--test` flag

Pass the flag to add test suite components to the targets, if specific
components are not identified. The `stack test` synonym sets this flag.

## Controlling when building occurs

### `--file-watch` flag

Pass the flag to rebuild your project every time a file changes. By default it
will take into account all files belonging to the targets you specify. See also
the `--watch-all` flag.

### `--file-watch-poll` flag

Like the `--file-watch` flag, but based on polling the file system instead of
using events to determine if a file has changed.

### `--watch-all` flag

[:octicons-tag-24: 2.5.1](https://github.com/commercialhaskell/stack/releases/tag/v2.5.1)

Pass the flag to rebuild your project every time any local file changes (from
project packages or from local dependencies). See also the `--file-watch` flag.

## Controlling what happens after building

### `--benchmark-arguments`, `--ba` option

`stack build --bench --benchmark-arguments=<argument(s)>` will pass the
specified argument, or arguments, to each benchmark when it is run.

Specified arguments are separated by spaces. Arguments can be unquoted (if they
do not contain space or `"` characters) or quoted (`""`). Quoted arguments can
include 'escaped' characters, escaped with an initial `\` character.

### `--exec` option

`stack build --exec "<command> [<argument(s)>]"` will run the specified command
after a successful build.

Specified arguments are separated by spaces. Arguments can be unquoted (if they
do not contain space or `"` characters) or quoted (`""`). Quoted arguments can
include 'escaped' characters, escaped with an initial `\` character.

### `--test-arguments`, `--ta` option

`stack build --test --test-arguments=<argument(s)>` will pass the specified
argument, or arguments, to each test when it is run. This option can be
specified multiple times.

Specified arguments are separated by spaces. Arguments can be unquoted (if they
do not contain space or `"` characters) or quoted (`""`). Quoted arguments can
include 'escaped' characters, escaped with an initial `\` character.

## Flags affecting GHC's behaviour

### `--[no-]executable-profiling` flag

Default: Disabled

Set the flag to enable executable profiling for TARGETs and all its
dependencies.

### `--[no-]executable-stripping` flag

Default: Enabled

Unset the flag to disable executable stripping for TARGETs and all its
dependencies.

### `--fast` flag

GHC has many flags that specify individual optimisations of the compiler. GHC
also uses its `-O*` flags to specify convenient 'packages' of GHC optimisation
flags. GHC's flags are evaluated from left to right and later flags can override
the effect of earlier ones.

If no GHC `-O*` type flag is specified, GHC takes that to mean "Please
compile quickly; I'm not over-bothered about compiled-code quality." GHC's `-O0`
flag reverts to the same settings as if no `-O*` flags had been specified.

Pass Stack's `--fast` flag to add `-O0` to the flags and options passed to GHC.
The effect of `--fast` can be overriden with Stack's
[`--ghc-options`](#-ghc-options-option) command line options.

!!! note

    With one exception, GHC's `-O` flag is always passed to GHC first (being
    Cabal's default behaviour). The exception is if Cabal's
    `--disable-optimization` flag or `--enable-optimization[=n]`, `-O[n]`
    options are used during the configure step of the Cabal build process; see
    Stack's [`configure-options`](yaml_configuration.md#configure-options) YAML
    configuration option.

### `--ghc-options` option

GHC command line options can be specified for a package in its Cabal file
(including one created from a `package.yaml` file). This option augments and, if
applicable (see below), overrides any such GHC command line options and those
specified in Stack's YAML configuration files - see the
[`ghc-options`](yaml_configuration.md#ghc-options) configuration option.

`stack build --ghc-options <ghc_options>` passes the specified command line
options to GHC, depending on Stack's
[`apply-ghc-options`](yaml_configuration.md#apply-ghc-options) YAML
configuration option. This option can be specified multiple times.

GHC's command line options are _order-dependent_ and evaluated from left to
right. Later options can override the effect of earlier ones. Any GHC command
line options for a package specified at Stack's command line are applied after
those specified in Stack's YAML configuration files.

### `--[no-]library-profiling` flag

Default: Disabled

Set the flag to enable library profiling for TARGETs and all its dependencies.

### `--[no-]library-stripping` flag

Default: Enabled

Unset the flag to disable library stripping for TARGETs and all its
dependencies.

### `--pedantic` flag

Pass the flag to build your project with the GHC options `-Wall` and `-Werror`.
`-Wall` turns on all warning options that indicate potentially suspicious code.
`-Werror` makes any warning into a fatal error.

### `--profile` flag

Pass the flag to enable profiling in libraries, executables, etc. for all
expressions, and generate a profiling report in tests or benchmarks.

### `--[no-]split-objs` flag

:octicons-beaker-24: Experimental

Default: Disabled

Set the flag to enable the GHC option `--split-objs`. This will reduce output
size (at the cost of build time).

!!! note

    The behaviour of this feature may be changed and improved. You will need to
    clean your project's Stack working directory before use. If you want to
    compile all dependencies with split-objs, you will need to delete the
    snapshot (and all snapshots that could reference that snapshot).

### `--no-strip` flag

Pass the flag to disable DWARF debugging symbol stripping in libraries,
executables, etc. for all expressions, producing larger executables but allowing
the use of standard debuggers/profiling tools/other utilities that use debugging
symbols.

### `--trace` flag

Pass the flag to enable profiling in libraries, executables, etc. for all
expressions, and generate a backtrace on exception.

## Flags affecting other tools' behaviour

### `--PROG-option` options

[:octicons-tag-24: 2.11.1](https://github.com/commercialhaskell/stack/releases/tag/v2.11.1)

`PROG` is a program recognised by Cabal (the library) and one of `alex`, `ar`,
`c2hs`, `cpphs`, `gcc`, `greencard`, `happy`, `hsc2hs`, `hscolour`, `ld`,
`pkg-config`, `strip` and `tar`.

`stack build --PROG-option <PROG_argument>` passes the specified command line
argument to `PROG`, if it used by Cabal during the configuration step. This
option can be specified multiple times. For example, if the program `happy` is
used by Cabal during the configuration step, you could command
`stack build --happy-option=--ghc` or `stack build --happy-option --ghc` to pass
to `happy` its `--ghc` flag.

By default, all and any `--PROG-option` options on Stack's command line are
applied to all local packages (targets or otherwise). This behaviour can be
changed. See the
[`apply-prog-options`](yaml_configuration.md#apply-prog-options) configuration
option.

Stack can also be configured to pass Cabal's `--PROG-option`, `--PROG-options`
or other options to Cabal during the configuration step. For further
information, see the documentation for the
[configure-options](yaml_configuration.md#configure-options) configuration
option.

## Flags relating to build outputs

### `--[no]-cabal-verbose` flag

Default: Disabled

Set the flag to enable verbose output from Cabal (the library). This flag is an
alternative to the `--cabal-verbosity` option.

### `--[no]-cabal-verbosity` option

`stack build --cabal-verbosity <verbosity_level>` sets the specified verbosity
level for output from Cabal (the library). It accepts Cabal's numerical and
extended syntax. This option is an alternative to setting the `--cabal-verbose`
flag.

### `--[no-]copy-bins` flag

[:octicons-tag-24: 0.1.3.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.3.0)

Default: Disabled

Set the flag to enable copying of built executable files (binaries) of targets
to Stack's local binary directory (see `stack path --local-bin`). The
`stack install` synonym sets this flag.

### `--[no-]copy-compiler-tool` flag

[:octicons-tag-24: 1.6.1](https://github.com/commercialhaskell/stack/releases/tag/v1.6.1)

Default: Disabled

Set the flag to enable copying of built executable files (binaries) of targets
to Stack's compiler tools binary directory (see
`stack path --compiler-tools-bin`).

### `--coverage` flag

Pass the flag to generate a code coverage report. For further information, see
the [code coverage](hpc_command.md) documentation.

### `--ddump-dir` option

GHC has a number of `ddump-*` flags and options to allow dumping out of
intermediate structures produced by the compiler. They include the
`-ddump-to-file` flag that causes the output from other flags to be dumped to a
file or files.

`stack build --ddump_dir <relative_directory>` causes Stack to copy `*.dump-*`
files to subdirectories of the specified directory, which is relative to Stack's
working directory for the project.

For example:

~~~text
stack build --ghc-options "-ddump-to-file -ddump-timings" --ddump-dir my-ddump-dir
~~~

### `--[no-]interleaved-output` flag

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
  standard error stream of the console as it happens.

* **More than one target package:** The build output from GHC (as opposed to
  from Stack) for each target package is sent to a log file for that package,
  unless an error occurs that prevents that. If color in output is in use, there
  will be two files, one with extension `.log` without color codes and one with
  extension `.log-color` with color codes. At the end of the build, the location
  of the directory containing the log files is reported. To also output the
  contents of the log files to the standard error output stream of the console
  at the end of the build, use Stack's `dump-logs` option. For further
  information about that option, see the
  [YAML configuration](yaml_configuration.md#dump-logs) documentation. The
  default `dump-logs` mode is to output the contents of any log files that
  include GHC warnings.

### `--[no]-open` flag

Default: Disabled

Set the flag to enable opening the local Haddock documentation in the browser.

## Other flags and options

### `--[no]-prefetch` flag

Default: Disabled

Set the flag to enable fetching packages necessary for the build immediately.
This can be useful with `stack build --dry-run`.

### `--progress-bar` option

[:octicons-tag-24: 2.13.1](https://github.com/commercialhaskell/stack/releases/tag/v2.13.1)

Default: `capped`

`stack build --progress-bar <format>` sets the format of the progress bar, where
`<format>` is one of `none` (no bar), `count-only` (only the package count is
displayed), `capped` (the bar showing package builds in progress is capped to a
length equal to the terminal width), and `full` (the bar is uncapped). On
terminals where 'backspace' has no effect if the cursor is in the first column,
bars longer than the terminal width will not be 'sticky' at the bottom of the
screen.

### `--tests-allow-stdin` flag

[:octicons-tag-24: 2.9.3](https://github.com/commercialhaskell/stack/releases/tag/v2.9.3)

Default: Enabled

Cabal defines a test suite interface
['exitcode-stdio-1.0'](https://hackage.haskell.org/package/Cabal-syntax-3.8.1.0/docs/Distribution-Types-TestSuiteInterface.html#v:TestSuiteExeV1.0)
where the test suite takes the form of an executable and the executable takes
nothing on the standard input stream (`stdin`). Pass this flag to override that
specification and allow the executable to receive input on that stream. If you
pass `--no-tests-allow-stdin` and the executable seeks input on the standard
input stream, an exception will be thown.

## Examples

All the following examples assume that:

*   if `stack build` is commanded outside of a project directory, there is no
    `stack.yaml` file in the current directory or ancestor directory and,
    consequently, the project-level configuration will be determined by a
    `stack.yaml` file in the `global-project` directory in the
    [Stack root](stack_root.md) (for further information, see the
    [YAML configuration](yaml_configuration.md) documentation); and

*   if `stack build` is commanded in a project directory, there is a
    `stack.yaml` file in that directory.

Examples:

*   In the project directory, `stack build --test --copy-bins` or, equivalently,
    `stack test --copy-bins` or `stack install --test`, will build libraries,
    executables, and test suites, run the test suites, and then copy the
    executables to Stack's local binary directory (see
    `stack path --local-bin`). This is an example of the flags composing.

*   The following example uses a clone of the
    `wai` [repository](https://github.com/yesodweb/wai/). The `wai` project
    comprises a number of packages, including `wai-extra` and `warp`. In the
    `wai` project directory, the command:

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

*   The following example uses the `Adga` package and assumes that `Adga-2.6.3`
    is the latest version in the package index (e.g. Hackage) and is not a
    version in the snapshot specified by the `stack.yaml` in the
    `global-project` directory in the Stack root.

    Outside a project directory, `stack build Adga-2.6.3 --copy-bins` or,
    equivalently, `stack install Agda-2.6.3`, will attempt to build the
    libraries and executables of the identified version of the package in the
    package index (using the `stack.yaml` file in the `global-project`
    directory in the Stack root), and then copy the executables to Stack's local
    binary directory (see `stack path --local-bin`).

    If a different snapshot is required to build the identified version of the
    package, then that can be specified at the command line. For example, to use
    the most recent Stackage Nightly snapshot:

    ~~~text
    stack --resolver nightly install Agda-2.6.3
    ~~~

    Alternatively, Stack can be used to unpack the package from the package
    index into a local project directory named after the package identifier (for
    further infomation, see the [`stack unpack` command](unpack_command.md)
    documentation) and, if the package does not provide its own Stack
    configuration file (`stack.yaml`), to attempt to initialise that
    configuration (for further information, see the
    [`stack init` command](init_command.md) documentation). For example:

    ~~~text
    stack unpack Agda-2.6.3
    cd Agda-2.6.3  # Change to the project directory
    stack init     # Attempt to create a project stack.yaml file
    stack install  # Equivalent to stack build --copy-bins
    ~~~
