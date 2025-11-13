<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Non-project-specific configuration

Non-project configuration options can be included in:

* a [project-level configuration file](../yaml/index.md#project-level-and-global-configuration-files)
  (`stack.yaml`, by default), like [project-specific](project.md) ones; or

* [global configuration files](../yaml/index.md#project-level-and-global-configuration-files)
  (`config.yaml`).

However, non-project-specific options in the project-level configuration file in
the `global-project` directory are ignored by Stack.

The options below are listed in alphabetic order.

## allow-different-user

[:octicons-tag-24: 1.0.1.0](https://github.com/commercialhaskell/stack/releases/tag/v1.0.1.0)

Restrictions: POSIX systems only.

Default: `false`

Command line equivalent (takes precedence): `--[no-]allow-different-user` flag

Allow users other than the owner of the [Stack root](../../topics/stack_root.md)
to use the Stack installation.

~~~yaml
allow-different-user: true
~~~

The intention of this option is to prevent file permission problems, for example
as the result of a Stack command executed under `sudo`.

The option is automatically enabled when Stack is re-spawned in a Docker
process.

## allow-newer

[:octicons-tag-24: 0.1.8.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.8.0)

Default: `false`

Command line equivalent (takes precedence):
[`stack build --[no-]allow-newer`](../../commands/build_command.md#-no-allow-newer-flag)
flag

Whether to ignore lower and upper version bounds in Cabal files.

!!! info

    The name `allow-newer` was chosen to match a commonly-used Cabal option
    which ignored only upper bounds.

~~~yaml
allow-newer: true
~~~

## allow-newer-deps

[:octicons-tag-24: 2.9.3](https://github.com/commercialhaskell/stack/releases/tag/v2.9.3)

Default: `none`

Determines a subset of packages to which `allow-newer` should apply. This option
has no effect (but warns) if `allow-newer` is `false`.

~~~yaml
allow-newer-deps:
  - foo
  - bar
~~~

## apply-ghc-options

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

Default: `locals`

Related command line:
[`stack build --ghc-options`](../../commands/build_command.md#-ghc-options-option)
option

Determines to which packages any GHC command line options specified on the
command line are applied. Possible values are: `everything` (all packages,
project packages or otherwise), `locals` (all project packages, targets or
otherwise), and `targets` (all project packages that are targets).

!!! note

    The use of `everything` can break invariants about your snapshot database.

!!! info

    Before Stack 0.1.6.0, the default value was `targets`.

## apply-prog-options

[:octicons-tag-24: 2.11.1](https://github.com/commercialhaskell/stack/releases/tag/v2.11.1)

Default: `locals`

Related command line:
[`stack build --PROG-option`](../../commands/build_command.md#-prog-option-options)
options

Determines to which packages all and any `--PROG-option` command line options
specified on the command line are applied. Possible values are: `everything`
(all packages, project packages or otherwise), `locals` (all project packages,
targets or otherwise), and `targets` (all project packages that are targets).

!!! note

    The use of `everything` can break invariants about your snapshot database.

## arch

Default: The machine architecture on which Stack is running.

Command line equivalent (takes precedence):
[`--arch`](../global_flags.md#-arch-option) option

Stack identifies different GHC executables by platform (operating system and
machine architecture), (optional) GHC variant and (optional) GHC build.
See [`setup-info`](#setup-info).

`arch` sets the machine architecture. Values can be those recognized by Cabal
(the library) (which are case-insensitive and include `i386`, `x86_64`, and
`aarch64` / `arm64`), or other values (which are case-sensitive and treated as
an unknown 'other' architecture of the specified name).

By default, Stack will warn the user if the specified machine architecture is an
unknown 'other' architecture. The warning can be muted; see
[`notify-if-arch-unknown`](#notify-if-arch-unknown)

!!! note

    The machine architecture on which Stack is running is as classified by
    Cabal (the library). Cabal does not distinguish between certain
    architectures. Examples are `ppc64`/`powerpc64`/`powerpc64le` (classified as
    `ppc64`) and `arm`/`armel`/`armeb` (classified as `arm`).

!!! note

    As Cabal (the library) does not distinguish between machine architectures
    `powerpc64` and `powerpc64le`, the latter can be specified in Stack's
    configuration as an 'other' architecture, such as `arch: ppc64le`.

## build

[:octicons-tag-24: 1.1.0](https://github.com/commercialhaskell/stack/releases/tag/v1.1.0)

Default:

~~~yaml
build:
  library-profiling: false
  executable-profiling: false
  library-stripping: true
  executable-stripping: true

  # NOTE: global usage of haddock can cause build failures when documentation is
  # incorrectly formatted. This could also affect scripts which use Stack.
  haddock: false
  haddock-arguments:

    # Additional arguments passed to haddock. The corresponding command line
    # option is --haddock-arguments. Example of use:
    #
    # haddock-args:
    # - "--css=/home/user/my-css"
    haddock-args: []

  # The corresponding command line flag is --[no-]open.
  open-haddocks: false

  # If Stack is configured to build Haddock documentation, defaults to true.
  haddock-deps: false

  # The configuration is ignored, if haddock-for-hackage: true.
  haddock-internal: false

  # The configuration is ignored, if haddock-for-hackage: true.
  haddock-hyperlink-source: true

  # If specified, implies haddock-internal: false and
  # haddock-hyperlink-source: true. Since Stack 2.15.1.
  haddock-for-hackage: false
  copy-bins: false
  copy-compiler-tool: false
  prefetch: false
  keep-going: false
  keep-tmp-files: false

  # These are inadvisable to use in your global configuration, as they make the
  # Stack build command line behave quite differently.
  force-dirty: false
  test: false
  test-arguments:
    rerun-tests: true   # Rerun successful tests

    # The corresponding command line option is --test-arguments. Example of use:
    #
    # additional-args:
    # - "--fail-fast"
    additional-args: []
    coverage: false
    no-run-tests: false
  bench: false
  benchmark-opts:

    # Example of use:
    #
    # benchmark-arguments: "--csv bench.csv"
    benchmark-arguments: ""
    no-run-benchmarks: false
  reconfigure: false
  cabal-verbosity: normal
  cabal-verbose: false
  split-objs: false
  skip-components: [] # --skip

  # Since Stack 1.8. Starting with Stack 2.0, the default is true
  interleaved-output: true

  # Since Stack 2.13.1. Available options are none, count-only, capped and full.
  progress-bar: capped

  # Since Stack 1.10.
  ddump-dir: ""
~~~

Command line equivalents (take precedence): Yes, see below.

Allows setting build options which are usually specified on the command line.

The meanings of these settings correspond directly with the command line flags
of the same name. For further information, see the
[`stack build`](../../commands/build_command.md) command documentation.

## casa

[:octicons-tag-24: 2.13.1](https://github.com/commercialhaskell/stack/releases/tag/v2.13.1)

Default:

~~~yaml
casa:
  enable: true # Use a Casa server?
  repo-prefix: https://casa.stackage.org # Unless casa-repo-prefix is set.
  max-keys-per-request: 1280 # Maximum number of keys per request.
~~~

This option specifies whether or not Stack should use a Casa
(content-addressable storage archive) server to cache Cabal files and all other
files in packages; and, if so, the prefix for the URL used to pull information
from the server and the maximum number of keys per request. For further
information, see this blog post about
[Casa and Stack](https://www.fpcomplete.com/blog/casa-and-stack/).

`repo-prefix` replaces [`casa-repo-prefix`](#casa-repo-prefix) (which is
deprecated) and has precedence if both keys are set.

## casa-repo-prefix

[:octicons-tag-24: 2.3.1](https://github.com/commercialhaskell/stack/releases/tag/v2.3.1)

Deprecated in favour of [`casa`](#casa), which takes precedence if present.

Default: `https://casa.stackage.org`

This option specifies the prefix for the URL used to pull information from the
Casa server.

## color

Command line equivalent (takes precedence): `--color` option

This option specifies when to use color in output. The option is used as
`color: <WHEN>`, where `<WHEN>` is 'always', 'never', or 'auto'. On Windows
versions before Windows 10, for terminals that do not support color codes, the
default is 'never'; color may work on terminals that support color codes.

(The British English spelling (colour) is also accepted. In configuration files,
the American spelling is the alternative that has priority.)

## compiler

[:octicons-tag-24: 0.1.8.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.8.0)

Command line equivalent (takes precedence): `--compiler` option

Overrides the version of the compiler (and, implicitly, its boot packages) in
the snapshot. Note that the `compiler-check` flag also applies to the version
numbers. This uses the same syntax as compiler snapshots like `ghc-9.10.3`. This
can be used to override the compiler (and, implicitly, its boot packages) for a
Stackage snapshot, like this:

~~~yaml
snapshot: lts-24.18
compiler: ghc-9.10.2
compiler-check: match-exact
~~~

:octicons-beaker-24: Experimental

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Stack also supports building the GHC compiler from source. For further
information, see the [building GHC from source](../../topics/GHC_from_source.md)
documentation.

## compiler-check

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

Default: `match-minor`

Specifies how the compiler version in the snapshot is matched against concrete
versions. Valid values:

* `match-minor`: make sure that the first three components match, but allow
  patch-level differences. For example< 7.8.4.1 and 7.8.4.2 would both match
  7.8.4. This is useful to allow for custom patch levels of a compiler.
* `match-exact`: the entire version number must match precisely
* `newer-minor`: the third component can be increased, e.g. if your snapshot is
  `ghc-7.10.1`, then 7.10.2 will also be allowed. This was the default up
  through Stack 0.1.3

## concurrent-tests

[:octicons-tag-24: 0.1.2.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.2.0)

Default: `true`

This option specifies whether test suites should be executed concurrently with
each other. The default is `true` since this is usually fine and it often means
that tests can complete earlier. However, if some test suites require exclusive
access to some resource, or require a great deal of CPU or memory resources,
then it makes sense to set this to `false`.

~~~yaml
concurrent-tests: false
~~~

## configure-options

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Related command line (takes precedence):
[`stack build --PROG-option`](../../commands/build_command.md#-prog-option-options)
options

`configure-options` can specify Cabal (the library) options (including
`--PROG-option` or `--PROG-options` options) for the configure step of the Cabal
build process for a named package, all project packages that are targets (using
the `$targets` key), all project packages (targets or otherwise) (using the
`$locals` key), or all packages (project packages or otherwise) (using the
`$everything` key).

~~~yaml
configure-options:
  $everything:
  - --with-gcc
  - /some/path
  $locals:
  - --happy-option=--ghc
  $targets:
  # Only works on platforms where GHC supports linking against shared Haskell
  # libraries:
  - --enable-executable-dynamic
  my-package:
  - --another-flag
~~~

On platforms where GHC supports linking against shared Haskell libraries (that
currently excludes Windows), Cabal's `--enable-executable-dynamic` flag (which
implies `--enable-shared`, unless `--disable-shared` is specified) links
dependent Haskell libraries into executables dynamically.

## connection-count

Default: `8`

Integer indicating how many simultaneous downloads are allowed to happen.

### default-init-snapshot

[:octicons-tag-24: 3.1.1](https://github.com/commercialhaskell/stack/releases/tag/v3.1.1)

Default: As for the [`stack init`](../../commands/init_command.md) command when
no snapshot is specified at the command line.

Command line equivalent (takes precedence):
[`--snapshot`](../global_flags.md#-snapshot-option) or
[`--resolver`](../global_flags.md#-resolver-option) option

This option specifies which snapshot to use with `stack init`, when none is
specified at the command line.

`default-init-snapshot: global` specifies the snapshot specified by the
project-level configuration file in the `global-project` directory in the
[Stack root](../../topics/stack_root.md#global-project-directory).

## default-template

Default: `new-template` in the
[stack-templates](https://github.com/commercialhaskell/stack-templates/)
repository.

This option specifies which template to use with `stack new`, when none is
specified. Other templates are listed in the
[stack-templates](https://github.com/commercialhaskell/stack-templates/)
repository. See the output of `stack templates`.

## docker

Command line equivalents: `--docker-*` flags and options (see
`stack --docker-help` for details).

For further information, see the
[Docker integration](../../topics/docker_integration.md#configuration)
documentation.

## dump-logs

[:octicons-tag-24: 1.3.0](https://github.com/commercialhaskell/stack/releases/tag/v1.3.0)

Default: `warning`

Command line equivalent (takes precedence): `--[no-]dump-logs` flag

In the case of *non-interleaved* output and *more than one* target package,
Stack sends the build output from GHC for each target package to a log file,
unless an error occurs that prevents that. For further information, see the
[`stack build --[no-]interleaved-output` flag](../../commands/build_command.md#-no-interleaved-output-flag)
documentation.

The value of the `dump-logs` key controls what, if any, log file content is sent
('dumped') to the standard error stream of the console at the end of the build.
Possible values are:

~~~yaml
dump-logs: none    # do not dump the content of any log files
dump-logs: warning # dump the content of any log files that include GHC warnings
dump-logs: all     # dump the content of all log files
~~~

At the command line, `--no-dump-logs` is equivalent to `dump-logs: none` and
`--dump-logs` is equivalent to `dump-logs: all`.

If GHC reports an error during the build and a log file is created, that build
output will be included in the log file. Stack will also report errors during
building to the standard error stream. That stream can be piped to a file. For
example, for a file named `stderr.log`:

~~~text
stack --no-dump-logs --color always build --no-interleaved-output 2> stderr.log
~~~

## extra-include-dirs

Default: `[]`

Command line equivalent: `--extra-include-dirs` option (repeat for each
directory)

A list of extra paths to be searched for header files. Paths should be absolute

~~~yaml
extra-include-dirs:
- /opt/foo/include
~~~

Since these are system-dependent absolute paths, it is recommended that you
specify these in your `config.yaml` file. If you control the build environment
in your project's ``stack.yaml``, perhaps through docker or other means, then it
may well make sense to include these there as well.

## extra-lib-dirs

Default: `[]`

Command line equivalent: `--extra-lib-dirs` option (repeat for each directory)

A list of extra paths to be searched for libraries. Paths should be absolute

~~~yaml
extra-lib-dirs:
- /opt/foo/lib
~~~

Since these are system-dependent absolute paths, it is recommended that you
specify these in your `config.yaml` file. If you control the build environment
in your project's ``stack.yaml``, perhaps through Docker or other means, then it
may well make sense to include these there as well.

## extra-path

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

This option specifies additional directories to prepend to the PATH. These will
be used when resolving the location of executables, and will also be visible in
the PATH of processes run by Stack.

For example, to prepend `/path-to-some-dep/bin` to your PATH:

~~~yaml
extra-path:
- /path-to-some-dep/bin
~~~

Other paths added by Stack - things like the project's binary directory and the
compiler's binary directory - will take precedence over those specified here
(the automatic paths get prepended).

## file-watch-hook

[:octicons-tag-24: 3.1.1](https://github.com/commercialhaskell/stack/releases/tag/v3.1.1)

Specifies the location of an executable or `sh` shell script to be run after
each attempted build with
[`build --file-watch`](../../commands/build_command.md#-file-watch-flag). An
absolute or relative path can be specified. Changes to the configuration after
the initial `build --file-watch` command are ignored.

If the project-level configuration is provided in the `global-project` directory
in the [Stack root](../../topics/stack_root.md), a relative path is assumed to
be relative to the current directory. Otherwise, it is assumed to be relative to
the directory of the project-level configuration file.

## ghc-build

[:octicons-tag-24: 1.3.0](https://github.com/commercialhaskell/stack/releases/tag/v1.3.0)

Default: `standard`

Command line equivalent (takes precedence): `--ghc-build` option

Stack identifies different GHC executables by platform (operating system and
machine architecture), (optional) GHC variant and (optional) GHC build.
See [`setup-info`](#setup-info).

`ghc-build` specifies a specialized architecture for the GHC executable.
Normally this is determined automatically, but it can be overridden. Possible
arguments include `standard`, `gmp4`, `nopie`, `tinfo6`, `tinfo6-libc6-pre232`,
`tinfo6-nopie`, `ncurses6`, `int-native` and `integersimple`.

## ghc-options

[:octicons-tag-24: 0.1.4.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.4.0)

Default: `{}`

Related command line (takes precedence):
[`stack build --ghc-options`](../../commands/build_command.md#-ghc-options-option)
option

GHC command line options can be specified for a package in its Cabal file
(including one created from a `package.yaml` file). This option augments and, if
applicable (see below), overrides any such GHC command line options.

`ghc-options` can specify GHC command line options for a named package, all
project packages that are targets (using the `$targets` key), all project
packages (targets or otherwise) (using the `$locals` key), or all packages
(project packages or otherwise) (using the `$everything` key).

~~~yaml
ghc-options:
  "$everything": -O2
  "$locals": -Wall
  "$targets": -Werror
  some-package: -DSOME_CPP_FLAG
~~~

GHC's command line options are _order-dependent_ and evaluated from left to
right. Later options can override the effect of earlier ones. Stack applies
options (as applicable) in the order of `$everything`, `$locals`, `$targets`,
and then those for the named package. Any GHC command line options for a package
specified at Stack's command line are applied after those specified in Stack's
configuration files.

Since Stack 1.6.1, setting a GHC options for a specific package will
automatically promote it to a project package (much like setting a custom
package flag). However, setting options via `$everything` on all flags will not
do so (see
[GitHub discussion](https://github.com/commercialhaskell/stack/issues/849#issuecomment-320892095)
for reasoning). This can lead to unpredictable behavior by affecting your
snapshot packages.

!!! info

    Before Stack 1.6.1, the key `*` (then deprecated) had the same function as
    the key `$everything`.

## ghc-variant

[:octicons-tag-24: 0.1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.5.0)

Default: `standard`

Command line equivalent (takes precedence): `--ghc-variant` option

Stack identifies different GHC executables by platform (operating system and
machine architecture), (optional) GHC variant and (optional) GHC build.
See [`setup-info`](#setup-info).

`ghc-variant` specifies a variant of the GHC executable. Known values are:

* `standard`: Use the standard GHC binary distribution
* `int-native`: From GHC 9.4.1, use a GHC bindist that uses the Haskell-native
   big-integer
  [backend](https://downloads.haskell.org/~ghc/9.0.2/docs/html/users_guide/9.0.1-notes.html#highlights).
  For further information, see this [article](https://iohk.io/en/blog/posts/2020/07/28/improving-haskells-big-numbers-support/).
* `integersimple`: Use a GHC bindist that uses
  [integer-simple instead of GMP](https://ghc.haskell.org/trac/ghc/wiki/ReplacingGMPNotes)
* any other value: Use a custom GHC bindist. You should specify
  [setup-info](#setup-info) or [setup-info-locations](#setup-info-locations)
  so `stack setup` knows where to download it,
  or pass the `stack setup --ghc-bindist` argument on the command-line

This option is incompatible with `system-ghc: true`.

## global-hints-location

[:octicons-tag-24: 3.1.1](https://github.com/commercialhaskell/stack/releases/tag/v3.1.1)

Default:

~~~yaml
global-hints-location:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/global-hints.yaml
~~~

(as set in the `pantry` library)

Sets the location of the global hints YAML file. The location can be either a
filepath of a local file or a URL. The filepath can be absolute or relative to
the Stack root.

For example:

~~~yaml
global-hints-location:
  filepath: \pantry\global-hints.yaml
~~~

or:

~~~yaml
global-hints-location:
  url: https://example.com/global-hints/location/global-hints.yaml
~~~

## hackage-base-url

[:octicons-tag-24: 1.9.1](https://github.com/commercialhaskell/stack/releases/tag/v1.9.1)

Default: `https://hackage.haskell.org/`

Sets the address of the Hackage server to upload the package to.

~~~yaml
hackage-base-url: https://hackage.example.com/
~~~

## hide-source-paths

Default: `true`
([:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1))

Whether to use the `-fhide-source-paths` option by default for GHC >= 8.2:

~~~yaml
hide-source-paths: false
~~~

Build output when enabled:

~~~text
...
[1 of 2] Compiling Lib
[2 of 2] Compiling Paths_test_pr
...
~~~

Build output when disabled:

~~~text
...
[1 of 2] Compiling Lib              ( src/Lib.hs, .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Lib.o )
...
~~~

## hide-th-loading

Default: `true`

Strip out the "Loading ..." lines from GHC build output, produced when using
Template Haskell.

## hpack-force

[:octicons-tag-24: 3.1.1](https://github.com/commercialhaskell/stack/releases/tag/v3.1.1)

Default: `false`

Command line equivalent (takes precedence):
[`--hpack-force` option](../global_flags.md#-no-hpack-force-flag)

Whether or not to allow Hpack to overwrite a Cabal file that was created by a
more recent version of Hpack or has been modified manually. By default,
Hpack 0.12.0 or later will decline to overwrite a Cabal file that was created by
a more recent version of Hpack and Hpack 0.20.0 or later will decline to
overwrite a Cabal file that has been modified manually.

## ignore-revision-mismatch

(Removed 1.11)

This flag was introduced in Stack 1.6, and removed on the move to Pantry. You
will receive a warning if this configuration value is set.

## install-ghc

Default: `true`
([:octicons-tag-24: 1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v1.5.0))

Command line equivalent (takes precedence):
[`--[no-]install-ghc`](../global_flags.md#-no-install-ghc-flag) flag

Whether or not to download and install GHC when necessary. On Windows,
`install-ghc: false` also disables the download and installation of the
Stack-supplied MSYS2 when necessary.

## install-msys

[:octicons-tag-24: 3.5.1](https://github.com/commercialhaskell/stack/releases/tag/v3.5.1)

Restrictions: Windows systems only

Default: same as the [`install-ghc`](#install-ghc) setting (including if that is
set on the command line)

Command line equivalent (takes precedence):
[`--[no-]install-msys`](../global_flags.md#-no-install-msys-flag) flag

If Stack is checking for the Stack-supplied MSYS2 when Stack is setting up the
environment, whether or not to download and install MSYS2 when necessary.

To skip entirely checking for the Stack-supplied MSYS2, see the documentation
for the [`skip-msys`](#skip-msys) configuration option.

## jobs

Default: the number of CPUs (cores) that the machine has.

Command line equivalent (takes precedence):
[`-j`, `--jobs` option](../global_flags.md#-jobs-or-j-option)

Specifies the number of concurrent jobs (principally, Stack actions during
building - see further below) to run.

When [building GHC from source](../../topics/GHC_from_source.md), specifies the
`-j[<n>]` flag of GHC's Hadrian build system.

In some circumstances, the default can cause some machines to run out of memory
during building. If those circumstances arise, specify `jobs: 1`.

This configuration option is distinct from GHC's own `-j[<n>]` flag, which
relates to parallel compilation of modules within a package.

## local-bin-path

Default (on Unix-like operating systems): `~/.local/bin`

Default (on Windows): `%APPDATA%\local\bin`

Command line equivalent (takes precedence): `--local-bin-path` option

Specifies the target directory for
[`stack build --copy-bins`](../../commands/build_command.md#-no-copy-bins-flag)
and `stack install`. An absolute or relative path can be specified.

If the project-level configuration is provided in the `global-project` directory
in the [Stack root](../../topics/stack_root.md), a relative path is assumed to
be relative to the current directory. Otherwise, it is assumed to be relative to
the directory of the project-level configuration file.

## local-programs-path

[:octicons-tag-24: 1.3.0](https://github.com/commercialhaskell/stack/releases/tag/v1.3.0)

This overrides the location of the Stack 'programs' directory, where tools like
GHC get installed. The path must be an absolute one.

Stack's defaults differ between Unix-like operating systems and Windows.

=== "Unix-like"

    Default: `programs` directory in the
    [Stack root](../../topics/stack_root.md).

=== "Windows"

    Default: `$Env:LOCALAPPDATA\Programs\stack`, if the `LOCALAPPDATA`
    environment variable exists. Otherwise, the `programs` directory in the
    [Stack root](../../topics/stack_root.md).

    The MSYS2 tool is also installed in the Stack 'programs' directory.

    !!! warning "Space character in the path to Stack's 'programs' directory"

        If there is a space character in the path to Stack's 'programs'
        directory this may cause problems:

        *   with building packages that make use of the GNU project's `autoconf`
            package and `configure` shell script files. That may be the case
            particularly if there is no corresponding short name ('8 dot 3'
            name) for the directory in the path with the space (which may be the
            case if '8 dot 3' names have been stripped or their creation not
            enabled by default). Examples of packages on Hackage that make use
            of `configure` are `network` and `process`; and

        *   building with GHC 9.4.1 and later. These versions of GHC have a bug
            which means they do not work if the path to the `ghc` executable has
            a space in it.

        The default location for Stack's 'programs' directory will have a space
        in the path if the value of the `USERNAME` environment variable includes
        a space.

        If there are problems building, it will be necessary to specify an
        alternative path that does not contain spaces. For example, the
        `programs` directory in the [Stack root](../../topics/stack_root.md)
        (assuming that path is space-free). For example, if the relevant
        directory is `C:\sr\programs`, add:
        ~~~yaml
        local-programs-path: C:\sr\programs
        ~~~

        to Stack's [global configuration](https://docs.haskellstack.org/en/stable/configure/yaml/#location-of-global-configuration)
        file (`config.yaml`).

        If that global configuration file does not yet exist, command:
        ~~~text
        stack --no-install-ghc setup
        ~~~

        to cause Stack to create it (without also installing GHC).

=== "Windows (Command Prompt)"

    Default: `%LOCALAPPDATA%\Programs\stack`, if the `LOCALAPPDATA`
    environment variable exists. Otherwise, the `programs` directory in the
    [Stack root](../../topics/stack_root.md).

    The MSYS2 tool is also installed in the Stack 'programs' directory.

    !!! warning "Space character in the path to Stack's 'programs' directory"

        If there is a space character in the path to Stack's 'programs'
        directory this may cause problems:

        *   with building packages that make use of the GNU project's `autoconf`
            package and `configure` shell script files. That may be the case
            particularly if there is no corresponding short name ('8 dot 3'
            name) for the directory in the path with the space (which may be the
            case if '8 dot 3' names have been stripped or their creation not
            enabled by default). Examples of packages on Hackage that make use
            of `configure` are `network` and `process`; and

        *   building with GHC 9.4.1 and later. These versions of GHC have a bug
            which means they do not work if the path to the `ghc` executable has
            a space in it.

        The default location for Stack's 'programs' directory will have a space
        in the path if the value of the `USERNAME` environment variable includes
        a space.

        If there are problems building, it will be necessary to specify an
        alternative path that does not contain spaces. For example, the
        `programs` directory in the [Stack root](../../topics/stack_root.md)
        (assuming that path is space-free). For example, if the relevant
        directory is `C:\sr\programs`, add:
        ~~~yaml
        local-programs-path: C:\sr\programs
        ~~~

        to Stack's [global configuration](https://docs.haskellstack.org/en/stable/configure/yaml/#location-of-global-configuration)
        file (`config.yaml`).

        If that global configuration file does not yet exist, command:
        ~~~text
        stack --no-install-ghc setup
        ~~~

        to cause Stack to create it (without also installing GHC).

## modify-code-page

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

Restrictions: Windows systems only.

Default: `true`

Command line equivalent (takes precedence): `--[no-]modify-code-page` flag

Whether to modify the code page for UTF-8 output.

~~~yaml
modify-code-page: false
~~~

## msys-environment

[:octicons-tag-24: 3.1.1](https://github.com/commercialhaskell/stack/releases/tag/v3.1.1)

Restrictions: Windows systems only.

Default: `MINGW64` (64-bit Windows) or `MINGW32` (32-bit Windows)

The name of the MSYS2 environment (case-sensitive) used in the Stack
environment. Valid environments are `CLANG32`, `CLANG64`, `CLANGARM64`,
`MINGW32`, `MINGW64`, and `UCRT64`.

## nix

[:octicons-tag-24: 0.1.10.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.10.0)

Default:

~~~yaml
nix:
  enable: false # Except on NixOS, where `enable: true`
  pure: true
  packages: []
  shell-file:
  nix-shell-options: []
  path: []
  add-gc-roots: false
~~~

Command line equivalents: `--nix-*` flags and options (see `stack --nix-help`
for details).

For further information, see the
[Nix integration](../../topics/nix_integration.md#non-project-specific-configuration) documentation.

## notify-if-arch-unknown

[:octicons-tag-24: 2.15.1](https://github.com/commercialhaskell/stack/releases/tag/v2.15.1)

Default: `true`

If the specified machine architecture value is unknown to Cabal (the library),
should Stack notify the user of that?

## notify-if-base-not-boot

:octicons-tag-24: UNRELEASED

Default: `true`

From GHC 9.12.1, `base` is not a GHC wired-in package. When using such GHC
versions, if the specified `base` package is other than the GHC boot package,
should Stack notify the user of that?

When using GHC versions where `base` is a GHC wired-in package, Stack will
always notify the user if the specified `base` package is other than the GHC
boot package.

## notify-if-cabal-untested

[:octicons-tag-24: 2.15.1](https://github.com/commercialhaskell/stack/releases/tag/v2.15.1)

Default: `true`

If Stack has not been tested with the version of Cabal (the library) that has
been found, should Stack notify the user of that?

## notify-if-ghc-untested

[:octicons-tag-24: 2.15.1](https://github.com/commercialhaskell/stack/releases/tag/v2.15.1)

Default: `true`

If Stack has not been tested with the version of GHC that is being used, should
Stack notify the user of that?

## notify-if-nix-on-path

[:octicons-tag-24: 2.15.1](https://github.com/commercialhaskell/stack/releases/tag/v2.15.1)

Default: `true`

If Stack's integration with the Nix package manager is not enabled, should Stack
notify the user if a `nix` executable is on the PATH?

## notify-if-no-run-benchmarks

[:octicons-tag-24: 3.3.1](https://github.com/commercialhaskell/stack/releases/tag/v3.3.1)

Default: `true`

Should Stack notify the user if the automatic running of a benchmark is
prevented by the `--no-run-benchmarks` flag?

## notify-if-no-run-tests

[:octicons-tag-24: 3.3.1](https://github.com/commercialhaskell/stack/releases/tag/v3.3.1)

Default: `true`

Should Stack notify the user if the automatic running of a test suite is
prevented by the `--no-run-tests` flag?

## package-index

[:octicons-tag-24: 2.9.3](https://github.com/commercialhaskell/stack/releases/tag/v2.9.3)

Default:

~~~yaml
package-index:
  download-prefix: https://hackage.haskell.org/
  hackage-security:
    keyids:
    - 0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d
    - 1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42
    - 51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921
    - c7de58fc6a224b92b5b513f26fbb8b370f2d97c7cfe0075a951314a55734be93
    - fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0
    key-threshold: 3
    ignore-expiry: true
~~~

Specify the package index. The index must use the
[Hackage Security](https://hackage.haskell.org/package/hackage-security) format.
This setting is most useful for providing a mirror of the official Hackage
server for

* bypassing a firewall; or
* faster downloads.

If the setting specifies an index that does not mirror Hackage, it is likely
that will result in significant breakage, including most snapshots failing to
work.

In the case of Hackage, the keys of its root key holders are contained in the
`haskell-infra/hackage-root-keys`
[repository](https://github.com/haskell-infra/hackage-root-keys). The Hackage
package index is signed. A signature is valid when three key holders have
signed. The Hackage timestamp is also signed. A signature is valid when one key
holder has signed.

If the `hackage-security` key is absent, the Hackage Security configuration will
default to that applicable to the official Hackage server since 2025-07-24.

`key-threshold` specifies the minimum number of keyholders that must have signed
the package index for it to be considered valid.

`ignore-expiry` specifies whether or not the expiration of timestamps should be
ignored.

!!! info

    In the case of Stack 3.7.1 and before, if the `hackage-security` key is
    absent, the Hackage Security configuration will default to that applicable
    to the official Hackage server before 2025-07-24.

    Before Stack 2.1.3, the default for `ignore-expiry` was `false`. For more
    information, see
    [issue #4928](https://github.com/commercialhaskell/stack/issues/4928).

## pvp-bounds

[:octicons-tag-24: 0.1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.5.0)

Default: `none`

Command line equivalent (takes precedence):
[`stack sdist --pvp-bounds`](../../commands/sdist_command.md) option or
[`stack upload --pvp-bounds`](../../commands/upload_command.md) option

!!! warning

    As of Stack 1.6.0, this feature does not reliably work, due to issues with
    the Cabal library's printer. Stack will generate a warning when a lossy
    conversion occurs, in which case you may need to disable this setting. For
    further information, see issue
    [#3550](https://github.com/commercialhaskell/stack/issues/3550).

When using the `sdist` and `upload` commands, this setting determines whether
the Cabal file's dependencies should be modified to reflect PVP lower and upper
bounds.

### Basic use

Values are `none` (unchanged), `upper` (add upper bounds), `lower` (add
lower bounds), and both (and upper and lower bounds). The algorithm Stack
follows is:

* If an upper or lower bound (other than `>= 0` - 'any version') already exists
  on a dependency, it is left alone
* When adding a lower bound, Stack looks at the current version specified by
  `stack.yaml`, and sets it as the lower bound (e.g., `foo >= 1.2.3`)
* When adding an upper bound, Stack sets it as less than the next major version
  (e.g., `foo < 1.3`)

~~~yaml
pvp-bounds: none
~~~

For further information, see the announcement
[blog post](https://www.fpcomplete.com/blog/2015/09/stack-pvp).

### Use with Cabal file revisions

[:octicons-tag-24: 1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v1.5.0)

Each of the values listed above supports adding `-revision` to the end of the
value, e.g. `pvp-bounds: both-revision`. This means that, when uploading to
Hackage, Stack will first upload your tarball with an unmodified Cabal file, and
then upload a Cabal file revision with the PVP bounds added.

This can be useful - especially combined with the
[Stackage no-revisions feature](http://www.snoyman.com/blog/2017/04/stackages-no-revisions-field) -
as a method to ensure PVP compliance without having to proactively fix bounds
issues for Stackage maintenance.

## recommend-stack-upgrade

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Default: `true`

When Stack notices that a new version of Stack is available, should it notify
the user?

## rebuild-ghc-options

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

Default: `false`

Should Stack rebuild a package when its GHC options change?

The default value reflects that, in most cases, GHC options are used to affect
optimization levels and warning behavior, for which GHC does not recompile the
modules.

!!! info

    Before Stack 0.1.6.0, Stack rebuilt a package when its GHC options changed.

## require-stack-version

Default: `"-any"`

Require a version of Stack within the specified range
([cabal-style](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information))
to be used for this project. Example: `require-stack-version: "== 0.1.*"`

## save-hackage-creds

[:octicons-tag-24: 1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v1.5.0)

Default: `true`

Command line equivalent (takes precedence):
[`stack upload --[no]-save-hackage-creds`](../../commands/upload_command.md)
option

Controls whether, when using `stack upload`, the user's Hackage username and
password are stored in a local file.

~~~yaml
save-hackage-creds: true
~~~

## setup-info

[:octicons-tag-24: 0.1.5.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.5.0)

The `setup-info` dictionary specifies download locations for tools to be
installed during set-up, such as GHC or, on Windows, 7z and MSYS2. The
dictionary maps `('Tool', 'Platform', 'Version')` to the location where it can
be obtained. For example, mapping `(GHC, 64-bit Windows, 9.2.3)` to the URL
hosting the archive file for GHC's installation.

Possible usages of this configuration option are:

1. Using Stack offline or behind a firewall.
2. Extending the tools known to Stack, such as cutting-edge versions of GHC or
   builds for custom Linux distributions (for use with the
   [ghc-variant](#ghc-variant) option).

By default, Stack obtains the dictionary from
[stack-setup-2.yaml](https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml).

The `setup-info` dictionary is constructed in the following order:

1. `setup-info` in the configuration - inline configuration
2. `--setup-info-yaml` command line arguments - URLs or paths. Multiple
   locations may be specified.
3. `setup-info-locations` in the configuration - URLs or paths. See further
   below.

The format of this key is the same as in the default
[stack-setup-2.yaml](https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml).
For example, GHC 9.2.3 of custom variant `myvariant` (see further below) on
64-bit Windows:

~~~yaml
setup-info:
  ghc:
    windows64-custom-myvariant:
      9.2.3:
        url: "https://example.com/ghc-9.2.3-x86_64-unknown-mingw32-myvariant.tar.xz"
~~~

'Platforms' are pairs of an operating system and a machine architecture (for
example, 32-bit i386 or 64-bit x86-64) (represented by the
`Cabal.Distribution.Systems.Platform` type). Stack currently (version 2.15.1)
supports the following pairs in the format of the `setup-info` key:

|Operating system|I386 arch|X86_64 arch|Other machine architectures                                 |
|----------------|---------|-----------|------------------------------------------------------------|
|Linux           |linux32  |linux64    |AArch64: linux-aarch64, Arm: linux-armv7, Sparc: linux-sparc|
|OSX             |macosx   |macosx     |                                                            |
|Windows         |windows32|windows64  |                                                            |
|FreeBSD         |freebsd32|freebsd64  |AArch64: freebsd-aarch64                                    |
|OpenBSD         |openbsd32|openbsd64  |                                                            |

For GHC, the distinguishing 'Version' in the key format includes a 'tag' for
any (optional) GHC variant (see [ghc-variant](#ghc-variant)) and a further 'tag'
for any (optional) specialised GHC build (see [ghc-build](#ghc-build)).

The optional variant 'tag' is either `-integersimple` or
`-custom-<custom_variant_name>`.

For example, for GHC 9.0.2 of specialised GHC build `tinfo6` on x86_64 Linux:
~~~yaml
setup-info:
  ghc:
    linux64-tinfo6:
      9.0.2:
        url: "http://downloads.haskell.org/~ghc/9.0.2/ghc-9.0.2a-x86_64-fedora27-linux.tar.xz"
        content-length: 237286244
        sha1: affc2aaa3e6a1c446698a884f56a0a13e57f00b4
        sha256: b2670e9f278e10355b0475c2cc3b8842490f1bca3c70c306f104aa60caff37b0
~~~

On Windows, the required 7z executable and DLL tools are represented in the
format of the `setup-info` key simply by `sevenzexe-info` and `sevenzdll-info`.

This configuration **adds** the specified setup information metadata to the
default. Specifying this configuration **does not** prevent the default
[stack-setup-2.yaml](https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml)
from being consulted as a fallback. If, however, you need to **replace** the
default `setup-info` dictionary, use the following:

~~~yaml
setup-info-locations: []
~~~

## setup-info-locations

[:octicons-tag-24: 2.3.1](https://github.com/commercialhaskell/stack/releases/tag/v2.3.1)

Command line equivalent (takes precedence): `--setup-info-yaml` option

By way of introduction, see the [`setup-info`](#setup-info) option. This option
specifies the location(s) of `setup-info` dictionaries.

The first location which provides a dictionary that specifies the location of a
tool - `('Tool', 'Platform', 'Version')` - takes precedence. For example, you
can extend the default tools, with a fallback to the default `setup-info`
location, as follows:

~~~yaml
setup-info-locations:
- C:/stack-offline/my-stack-setup.yaml
- relative/inside/my/project/setup-info.yaml
- \\smbShare\stack\my-stack-setup.yaml
- http://stack-mirror.com/stack-setup.yaml
# Fallback to the default location
- https://github.com/commercialhaskell/stackage-content/raw/master/stack/stack-setup-2.yaml
~~~

Stack only refers to the default `setup-info` location if no locations are
specified in the `setup-info-locations` configuration or on the command line
using the `--setup-info-yaml` option.

For example, both of the following will cause `stack setup` not to consult the
default `setup-info` location:

~~~yaml
setup-info-locations:
- C:/stack-offline/my-stack-setup.yaml
~~~

and

~~~yaml
setup-info-locations: []
~~~

Relative paths are resolved relative to the `stack.yaml` file (either the one in
the local project or the global `stack.yaml`).

Relative paths may also be used for the installation paths to tools (such as GHC
or 7z). This allows vendoring the tools inside a monorepo (a single repository
storing many projects). For example:

Directory structure:

~~~text
- src/
- installs/
  - my-stack-setup.yaml
  - 7z.exe
  - 7z.dll
  - ghc-9.2.3.tar.xz
- stack.yaml
~~~

In the project's `stack.yaml`:

~~~yaml
setup-info-locations:
- installs/my-stack-setup.yaml
~~~

In `installs/my-stack-setup.yaml`:

~~~yaml
sevenzexe-info:
  url: "installs/7z.exe"

sevenzdll-info:
  url: "installs/7z.dll"

ghc:
  windows64:
    9.2.3:
      url: "installs/ghc-9.2.3.tar.xz"
~~~

## skip-ghc-check

Default: `false`

Command line equivalent (takes precedence): `--[no-]skip-ghc-check` flag

Should we skip the check to confirm that your system GHC version (on the PATH)
matches what your project expects?

## skip-msys

[:octicons-tag-24: 0.1.2.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.2.0)

Restrictions: Windows systems only

Default: `false`

Command line equivalent (takes precedence):
[`--[no-]skip-msys`](../global_flags.md#-no-skip-msys-option) flag

Skips checking for the Stack-supplied MSYS2 (and installing that MSYS2, if it is
not installed) when Stack is setting up the environment.

To prevent installation of MSYS2, if it is not installed, see the documentation
for the [`install-msys`](#install-msys) configuration option.

~~~yaml
skip-msys: true
~~~

!!! note

    Usually, the use of this option does not make sense in project-level
    configuration and it is used only in global configuration.

## snapshot-location-base

[:octicons-tag-24: 2.5.1](https://github.com/commercialhaskell/stack/releases/tag/v2.5.1)

Default: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/
(as set in the `pantry` library)

Command line equivalent (takes precedence):
[`--snapshot-location-base`](../global_flags.md#-snapshot-location-base-command-option)
option

Sets the base location of the LTS Haskell or Stackage Nightly snapshots.

For example:

~~~yaml
snapshot-location-base: https://example.com/snapshots/location/
~~~

has the following effect:

* `lts-X.Y` expands to `https://example.com/snapshots/location/lts/X/Y.yaml`
* `nightly-YYYY-MM-DD` expands to
  `https://example.com/snapshots/location/nightly/YYYY/M/D.yaml`

This key is convenient in setups that restrict access to GitHub, for instance
closed corporate setups. In this setting, it is common for the development
environment to have general access to the internet, but not for testing/building
environments. To avoid the firewall, one can run a local snapshots mirror and
then use a custom `snapshot-location-base` in the closed environments only.

## stack-colors

Command line equivalent (takes precedence): `--stack-colors` option

Stack uses styles to format some of its output. The default styles do not work
well with every terminal theme. This option specifies Stack's output styles,
allowing new styles to replace the defaults. The option is used as
`stack-colors: <STYLES>`, where `<STYLES>` is a colon-delimited sequence of
key=value, 'key' is a style name and 'value' is a semicolon-delimited list of
'ANSI' SGR (Select Graphic Rendition) control codes (in decimal). Use the
command `stack ls stack-colors --basic` to see the current sequence.

The 'ANSI' standards refer to (1) standard ECMA-48 'Control Functions for Coded
Character Sets' (5th edition, 1991); (2) extensions in ITU-T Recommendation
(previously CCITT Recommendation) T.416 (03/93) 'Information Technology – Open
Document Architecture (ODA) and Interchange Format: Character Content
Architectures' (also published as ISO/IEC International Standard 8613-6); and
(3) further extensions used by 'XTerm', a terminal emulator for the X Window
System. The 'ANSI' SGR codes are described in a
[Wikipedia article](http://en.wikipedia.org/wiki/ANSI_escape_code)
and those codes supported on current versions of Windows in
[Microsoft's documentation](https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences).

For example, users of the popular
[Solarized Dark](https://ethanschoonover.com/solarized/)
terminal theme might wish to set the styles as follows:

~~~yaml
stack-colors: error=31:good=32:shell=35:dir=34:recommendation=32:target=95:module=35:package-component=95:secondary=92:highlight=32
~~~
In respect of styles used in verbose output, some of that output occurs before
the configuration file is processed.

(The British English spelling (colour) is also accepted. In configuration files,
the American spelling is the alternative that has priority.)

## stack-developer-mode

[:octicons-tag-24: 2.3.3](https://github.com/commercialhaskell/stack/releases/tag/v2.3.3)

Default (official distributed binaries): `false`

Default (built from source): `true`

Turns on a mode where some messages are printed at WARN level instead of DEBUG
level, especially useful for developers of Stack itself.

~~~yaml
stack-developer-mode: false
~~~

## system-ghc

Default: `false`, unless the [Docker](../../topics/docker_integration.md) or
[Nix](../../topics/nix_integration.md) integration is enabled.

Command line equivalent (takes precedence): `--[no-]system-ghc` flag

Enables or disables using the GHC available on the PATH. (Make sure PATH is
explicit, i.e., do not use ~.) Useful to enable if you want to save the time,
bandwidth or storage space needed to setup an isolated GHC.

In a Nix-enabled configuration, Stack is incompatible with `system-ghc: false`.

~~~yaml
# Turn on system GHC
system-ghc: true
~~~

## templates

Command line equivalent (takes precedence): `stack new --param <key>:<value>`
(or `-p`) option

Templates used with `stack new` have a number of parameters that affect the
generated code. These can be set for all new projects you create. The result of
them can be observed in the generated LICENSE and Cabal files. The value for all
of these parameters must be strings.

The parameters are: `author-email`, `author-name`, `category`, `copyright`,
`year` and `github-username`.

* _author-email_ - sets the `maintainer` property in Cabal
* _author-name_ - sets the `author` property in Cabal and the name used in
  LICENSE
* _category_ - sets the `category` property in Cabal. This is used in Hackage.
  For examples of categories see
  [Packages by category](https://hackage.haskell.org/packages/). It makes sense
  for `category` to be set on a per project basis because it is uncommon for all
  projects a user creates to belong to the same category. The category can be
  set per project by passing `-p "category:value"` to the `stack new` command.
* _copyright_ - sets the `copyright` property in Cabal. It is typically the
  name of the holder of the copyright on the package and the year(s) from which
  copyright is claimed. For example: `Copyright (c) 2023-2025 Joe Bloggs`
* _year_ - if `copyright` is not specified, `year` and `author-name` are used
  to generate the copyright property in Cabal. If `year` is not specified, it
  defaults to the current year.
* _github-username_ - used to generate `homepage` and `source-repository` in
  Cabal. For instance `github-username: myusername` and
  `stack new my-project new-template` would result:

~~~yaml
homepage: http://github.com/myusername/my-project#readme

source-repository head
  type: git
  location: https://github.com/myusername/my-project
~~~

These properties can be set in `config.yaml` as follows:
~~~yaml
templates:
  params:
    author-name: Your Name
    author-email: youremail@example.com
    category: Your Projects Category
    copyright: 'Copyright (c) 2025 Your Name'
    github-username: yourusername
~~~

Additionally, `stack new` can automatically initialize source control
repositories in the directories it creates.  Source control tools can be
specified with the `scm-init` option. At the moment, only `git` is supported.

~~~yaml
templates:
  scm-init: git
~~~

## urls

Default:

~~~yaml
urls:
  latest-snapshot: https://stackage-haddock.haskell.org/snapshots.json
~~~

Customize the URLs where Stack looks for snapshot build plans.

!!! note

    The default for Stack 1.3.0 to 2.15.3 was
    https://s3.amazonaws.com/haddock.stackage.org/snapshots.json. Following the
    handover of the Stackage project to the Haskell Foundation in
    early 2024, the file at that URL may not be up to date. Users of those
    versions of Stack should configure the URL to be the default above.

## with-gcc

Command line equivalent (takes precedence): `--with-gcc` option

Specify a path to GCC explicitly, rather than relying on the normal path
resolution.

~~~yaml
with-gcc: /usr/local/bin/gcc-5
~~~

## with-hpack

Command line equivalent (takes precedence): `--with-hpack` option

Use an [Hpack](https://github.com/sol/hpack) executable, rather than Stack's
in-built version of the Hpack functionality.

~~~yaml
with-hpack: /usr/local/bin/hpack
~~~

## work-dir

[:octicons-tag-24: 0.1.10.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.10.0)

Default: `.stack-work`

Command line equivalent (takes precedence):
[`--work-dir`](../global_flags.md#-work-dir-option) option

Environment variable alternative (lowest precedence):
[`STACK_WORK`](../environment_variables.md#stack_work)

`work-dir` specifies the path of Stack's work directory, within a local project
or package directory. The path must be a relative one, relative to the
root directory of the project or package. The relative path cannot include a
`..` (parent directory) component.
