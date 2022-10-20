<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# User guide (advanced)

Some of Stack's features will not be needed regularly or by all users. This part
of the guide provides information about those features. Some of the features are
complex and separate pages are dedicated to them.

## The `stack --hpack-numeric-version` flag

Stack will report the numeric version of its built-in Hpack library to standard
output (e.g. `0.35.0`) and quit.

## The `stack --numeric-version` flag

Stack will report its numeric version to standard output (e.g. `2.9.1`) and
quit.

## The `stack --[no-]rsl-in-log` flag

[:octicons-tag-24: 2.9.1](https://github.com/commercialhaskell/stack/releases/tag/v2.9.1)

Default: Disabled

Enables/disables the logging of the raw snapshot layer (rsl) in debug output.
Information about the raw snapshot layer can be lengthy. If you do not need it,
it is best omitted from the debug output.

## The `stack --silent` flag

Equivalent to the `stack --verbosity silent` option.

## The `stack --stack-root` option

`stack --stack-root <absolute_path_to_the_Stack_root>` specifies the path to the
Stack root directory. The path must be an absolute one. The option will override
the contents of any `STACK_ROOT` environment variable.

## The `stack --[no-]time-in-logs` flag

Default: Enabled

Enables/disables the inclusion of time stamps against logging entries when the
verbosity level is 'debug'.

## The `stack -v, --verbose` flags

Equivalent to the `stack --verbosity debug` option.

## The `stack --verbosity` option

Default: info

`stack --verbosity <log_level>` will set the level for logging. Possible levels
are `silent`, `error`, `warn`, `info` and `debug`, in order of increasing
amounts of information provided by logging.

## The `stack --version` flag

Stack will report its version to standard output and quit. For versions that are
release candidates, the report will list the dependencies that Stack has been
compiled with.

## The `stack --work-dir` option

Default: `.stack-work`

`stack --work-dir <relative_path_to_the_Stack_root>` specifies the path to
Stack's work directory for the project. The path must be a relative one,
relative to the project's root directory. The option will override the contents
of any `STACK_WORK` environment variable.

## The `stack build` command

The `stack build` command is introduced in the first part of Stack's
[user's guide](GUIDE.md#the-stack-build-command). For further information about
the command, see the [build command](build_command.md) documentation.

## The `stack config` commands

The `stack config` commands provide assistance with accessing or modifying
Stack's configuration. See `stack config` for the available commands.

## The `stack config env` command

`stack config env` outputs a script that sets or unsets environment variables
for a Stack environment. Flags modify the script that is output:

* `--[no-]locals` (enabled by default) include/exclude local package information
* `--[no-]ghc-package-path` (enabled by default) set `GHC_PACKAGE_PATH`
  environment variable or not
* `--[no-]stack-exe` (enabled by default) set `STACK_EXE` environment variable
  or not
* `--[no-]locale-utf8` (disabled by default) set the `GHC_CHARENC`
  environment variable to `UTF-8` or not
* `--[no-]keep-ghc-rts` (disabled by default) keep/discard any `GHCRTS`
  environment variable

## The `stack config set` commands

The `stack config set` commands allow the values of keys in YAML configuration
files to be set. See `stack config set` for the available keys.

## The `stack config set install-ghc` command

`stack config set install-ghc true` or `false` sets the `install-ghc` key in a
YAML configuration file, accordingly. By default, the project-level
configuration file (`stack.yaml`) is altered. The `--global` flag specifies the
user-specific global configuration file (`config.yaml`).

## The `stack config set package-index download-prefix` command

:octicons-tag-24: Unreleased

`stack config set package-index download-prefix <url>` sets the
`download-prefix` key of the `package-index` key in a YAML configuration file,
accordingly. By default, the project-level configuration file (`stack.yaml`) is
altered. The `--global` flag specifies the user-specific global configuration
file (`config.yaml`).

## The `stack config set resolver` command

`stack config set resolver <snapshot>` sets the `resolver` key in the
project-level configuration file (`stack.yaml`).

A snapshot of `lts` or `nightly` will be translated into the most recent
available. A snapshot of `lts-19` will be translated into the most recent
available in the `lts-19` sequence.

Known bug:

* The command does not respect the presence of a `snapshot` key.

## The `stack config set system-ghc` command

`stack config set system-ghc true` or `false` sets the `system-ghc` key in a
YAML configuration file, accordingly. By default, the project-level
configuration file (`stack.yaml`) is altered. The `--global` flag specifies the
user-specific global configuration file (`config.yaml`).

## The `stack dot` command

If you'd like to get some insight into the dependency tree of your packages, you
can use the `stack dot` command and Graphviz. More information is available in
the [Dependency visualization](dependency_visualization.md) documentation.

## The `stack ide` commands

The `stack ide` commands provide information that may be of use in an
integrated development environment (IDE). See `stack ide` for the available
commands.

## The `stack ide packages` command

`stack ide packages` lists all available local packages that are loadable. By
default, its output is sent to the standard error channel. This can be changed
to the standard output channel with the `--stdout` flag.

By default, the output is the package name (without its version). This can be
changed to the full path to the package's Cabal file with the `--cabal-files`
flag.

## The `stack ide targets` command

`stack ide targets` lists all available Stack targets. By default, its output is
sent to the standard error channel. This can be changed to the standard output
channel with the `--stdout` flag.

For example, for the Stack project itself, command:

~~~text
cd stack
stack ide targets
~~~

and the output from the second command is:

~~~text
stack:lib
stack:exe:stack
stack:exe:stack-integration-test
stack:test:stack-test
~~~

## The `stack list` command

[:octicons-tag-24: 2.7.1](https://github.com/commercialhaskell/stack/releases/tag/v2.7.1)

`stack list <package_name>` will list the latest version of the package from
Hackage. If the package name cannot be found on Hackage, even after updating the
package index, suggestions (not necessarily good ones) will be made about the
intended package name.

`stack --resolver <snapshot> <package_name>` will list the version of the
package in the specified snapshot, unless the package comes with GHC on
Unix-like operating systems. If the package name cannot be found in the
snapshot, the command will fail, identifying only the package(s) that did not
appear in the snapshot.

More than one package name can be specified.

`stack --resolver <snapshot>` will list all the packages in the specified
snapshot, except those which come with GHC on Unix-like operating systems.

For example:

~~~text
stack list base unix Win32 acme-missiles pantry
base-4.17.0.0
unix-2.8.0.0
Win32-2.13.3.0
acme-missiles-0.3
pantry-0.5.7

stack list paltry
Could not find package paltry, updating
...
Package index cache populated
- Could not find package paltry on Hackage. Perhaps you meant: pretty, pasty, xattr, alloy, para, pappy, alure, polar, factory, pastis

stack --resolver lts-19.25 base unix Win32 acme-missiles pantry
- Package does not appear in snapshot: base
- Package does not appear in snapshot: unix
- Package does not appear in snapshot: acme-missiles

stack --resolver lts-19.25 Win32 pantry
Win32-2.12.0.1
pantry-0.5.7

stack --resolver lts-19.25
AC-Angle-1.0
ALUT-2.4.0.3
...
zstd-0.1.3.0
ztail-1.2.0.3
~~~

## The `stack ls` commands

The `stack ls` commands list different types of information. See `stack ls` for
the available commands.

## The `stack ls dependencies` command

`stack ls dependencies` lists all of the packages and versions used for a
project.

## The `stack ls snapshots` command

`stack ls snapshots` will list all the local snapshots by default. You can also
view the remote snapshots using `stack ls snapshots remote`. It also supports
options for viewing only lts (`-l`) and nightly (`-n`) snapshots.

## The `stack ls stack-colors` command

The British English spelling is also accepted (`stack ls stack-colours`).

`stack ls stack-colors` will list all of Stack's output styles. A number of
different formats for the output are available, see
`stack ls stack-colors --help`.

The default is a full report, with the equivalent SGR instructions and an
example of the applied style. The latter can be disabled with flags `--no-sgr`
and `--no-example`.

The flag `--basic` specifies a more basic report, in the format that is accepted
by Stack's command line option `--stack-colors` and the YAML configuration key
`stack-colors`.

## The `stack ls tools` command

`stack ls tools` will list Stack's installed tools. On Unix-like operating
systems, they will be one or more versions of GHC. On Windows, they will include
MSYS2. For example, on Windows the command:

~~~text
stack ls tools
~~~

yields output like:

~~~text
ghc-9.4.1
ghc-9.2.4
ghc-9.0.2
msys2-20210604
~~~

The `--filter <tool_name>` option will filter the output by a tool name (e.g.
'ghc', 'ghc-git' or 'msys2'). The tool name is case sensitive. For example the
command:

~~~text
stack ls tools --filter ghc
~~~

yields output like:

~~~text
ghc-9.4.1
ghc-9.2.4
ghc-9.0.2
~~~

## The `stack query` command

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

`stack query` outputs certain build information. For example, for a
multi-package project `multi` specifying snapshot `lts-19.25` (GHC 9.0.2) and
with two local packages, `my-package-A` (version 0.1.0.0) and `my-package-B`
(version 0.2.0.0), command `stack query` outputs:

~~~text
compiler:
  actual: ghc-9.0.2
  wanted: ghc-9.0.2
locals:
  my-package-A:
    path: <absolute_path_to>\multi\my-package-A\
    version: 0.1.0.0
  my-package-B:
    path: <absolute_path_to>\multi\my-package-B\
    version: 0.2.0.0
~~~

The component parts of the information can be specified using 'selectors' with
the command. In the example above the selectors include `compiler`,
`compiler actual`, `locals`, `locals my-package-A`, and
`locals my-package-A version`. For example, commanding:

~~~text
stack query locals my-package-B path
~~~

results in output:

~~~text
<absolute_path_to>\multi\my-package-B\
~~~

## The `stack run` command

`stack run` builds a project executable and runs it. If the command has a first
argument and it is recognised as an executable target then that is built.
Otherwise, the project's first executable is built. If the project has no
executables Stack reports no executables found as an error.

Everything after `--` on the command line is interpreted as a command line
argument to be passed to what is run, other than a first argument recognised as
an executable target.

By default, the `GHC_PACKAGE_PATH` environment variable is set for the
subprocess. Pass the `--no-ghc-package-path` flag to not set the variable.

By default, the `STACK_EXE` environment variable is set with the path to Stack.
Pass the `--no-stack-exe` flag to not set the variable.

The `--cwd` option can be used to set the working directory before the
executable is run.

The `--rts-options` option (which can be specified multiple times) can be used
to pass a list of GHC's
[runtime system (RTS) options](https://downloads.haskell.org/~ghc/latest/docs/users_guide/runtime_control.html#)
to the executable when it is run. (The `+RTS` and `-RTS` must not be included.)

The `--package` option (which can be specified multiple times) can be used to
add a package name to build targets.

## The `stack sdist` command

Hackage only accepts packages for uploading in a standard form, a compressed
archive ('tarball') in the format produced by Cabal's `sdist` action.

`stack sdist` generates a file for your package, in the format accepted by
Hackage for uploads. The command will report the location of the generated file.

### The `stack sdist --ignore-check` flag

Pass the flag to disable checks of the package for common mistakes. By default,
the command will check the package for common mistakes.

### The `stack sdist --pvp-bounds` option

The `--pvp-bounds <pvp_bounds_mode>` option determines whether and, if so, how
PVP version bounds should be added to the Cabal file of the package. The
available modes for basic use are: `none`, `lower`, `upper`, and `both`. The
available modes for use with Cabal file revisions are `lower-revision`,
`upper-revision` and `both-revision`.

For futher information, see the
[YAML configuration](yaml_configuration.md#pvp-bounds) documentation.

### The `stack sdist --tar-dir` option

The `--tar-dir <path_to_directory>` option determines whether the package
archive should be copied to the specified directory.

### The `stack sdist --[no-]test-tarball` flag

Default: Disabled

Set the flag to cause Stack to test the resulting package archive, by attempting
to build it.

## The `stack setup` command

`stack setup` attempts to install a version of GHC - by default, the version
required by the project and only if it is not already available to Stack. For
example:

~~~text
stack setup
stack will use a sandboxed GHC it installed
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
~~~

Alternatively, the version of GHC to be installed can be specified as an
argument. For example `stack setup 9.0.2`.

Set the `--reinstall` flag (disabled by default) to attempt to install the
version of GHC regardless of whether it is already available to Stack.

The `--ghc-bindist <url>` option can be used to specify the URL of the GHC to be
downloaded and installed. This option requires the use of the `--ghc-variant`
option specifying a custom GHC variant.

If Stack is configured not to install GHC (`install-ghc: false` or passing the
`--no-install-ghc` flag) then `stack setup` will silently ignore the flag.

## The `stack templates` command

`stack templates` provides information about how to find templates available for
`stack new`.

Stack provides multiple templates to start a new project from. You can specify
one of these templates following your project name in the `stack new` command:

~~~text
stack new my-rio-project rio
Downloading template "rio" to create project "my-rio-project" in my-rio-project/ ...
Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- my-rio-project/

Selecting the best among 18 snapshots...

* Matches ...

Selected resolver: ...
Initialising configuration using resolver: ...
Total number of user packages considered: 1
Writing configuration to file: my-rio-project/stack.yaml
All done.
<Stack root>\templates\rio.hsfiles:   10.10 KiB downloaded...
~~~

The default templates repository is
https://github.com/commercialhaskell/stack-templates. You can download templates
from a different GitHub user by prefixing the username and a slash. Command:

~~~text
stack new my-yesod-project yesodweb/simple
~~~

Then template file `simple.hsfiles` would be downloaded from GitHub repository
`yesodweb/stack-templates`.

You can even download templates from a service other that GitHub, such as
[GitLab](https://gitlab.com) or [Bitbucket](https://bitbucket.com). For example,
command:

~~~text
stack new my-project gitlab:user29/foo
~~~

Template file `foo.hsfiles` would be downloaded from GitLab, user account
`user29`, repository `stack-templates`.

If you need more flexibility, you can specify the full URL of the template.
Command:

~~~text
stack new my-project https://my-site.com/content/template9.hsfiles
~~~

(The `.hsfiles` extension is optional; it will be added if it's not specified.)

Alternatively you can use a local template by specifying the path. Command:

~~~text
stack new project <path_to_template>/template.hsfiles
~~~

As a starting point for creating your own templates, you can use the
["simple" template](https://github.com/commercialhaskell/stack-templates/blob/master/simple.hsfiles).
The
[stack-templates repository](https://github.com/commercialhaskell/stack-templates#readme)
provides an introduction into creating templates.

## The `stack update` command

`stack update` will download the most recent set of packages from your package
indices (e.g. Hackage). Generally, Stack runs this for you automatically when
necessary, but it can be useful to do this manually sometimes.

## The `stack upgrade` command

`stack upgrade` will build a new version of Stack from source.
  * `--git` is a convenient way to get the most recent version from the `master`
     branch, for those testing and living on the bleeding edge.

## The `stack unpack` command

`stack unpack` does what you'd expect: downloads a tarball and unpacks it. It
accepts an optional `--to` argument to specify the destination directory.

## The `stack upload` command

Hackage accepts packages for uploading in a standard form, a compressed archive
('tarball') in the format produced by Cabal's `sdist` action.

`stack upload` generates a file for your package, in the format accepted by
Hackage for uploads, and uploads the package to Hackage. For example, if the
current working directory is the root directory of your project:

~~~text
stack upload .
~~~

### The `HACKAGE_USERNAME` and `HACKAGE_PASSWORD` environment variables

[:octicons-tag-24: 2.3.1](https://github.com/commercialhaskell/stack/releases/tag/v2.3.1)

`stack upload` will request a Hackage username and password to authenticate.
This can be avoided by setting the `HACKAGE_USERNAME` and `HACKAGE_PASSWORD`
environment variables. For
example:

=== "Unix-like"

    ~~~text
    export $HACKAGE_USERNAME="<username>"
    export $HACKAGE_PASSWORD="<password>"
    stack upload .
    ~~~

=== "Windows (with PowerShell)"

    ~~~text
    $Env:HACKAGE_USERNAME='<username>'
    $Env:HACKAGE_PASSWORD='<password>'
    stack upload .
    ~~~

### The `HACKAGE_KEY` environment variable

[:octicons-tag-24: 2.7.5](https://github.com/commercialhaskell/stack/releases/tag/v2.7.5)

Hackage allows its members to register an API authentification token and to
authenticate using the token.

A Hackage API authentification token can be used with `stack upload` instead of
username and password, by setting the `HACKAGE_KEY` environment variable. For
example:

=== "Unix-like"

     ~~~text
     HACKAGE_KEY=<api_authentification_token>
     stack upload .
     ~~~

=== "Windows (with PowerShell)"

     ~~~text
     $Env:HACKAGE_KEY=<api_authentification_token>
     stack upload .
     ~~~

### The `stack upload --candidate` flag

Pass the flag to upload a
[package candidate](http://hackage.haskell.org/upload#candidates).

### The `stack upload --ignore-check` flag

Pass the flag to disable checks of the package for common mistakes. By default,
the command will check the package for common mistakes.

### The `stack upload --pvp-bounds` option

The `--pvp-bounds <pvp_bounds_mode>` option determines whether and, if so, how
PVP version bounds should be added to the Cabal file of the package. The
available modes for basic use are: `none`, `lower`, `upper`, and `both`. The
available modes for use with Cabal file revisions are `lower-revision`,
`upper-revision` and `both-revision`.

For futher information, see the
[YAML configuration](yaml_configuration.md#pvp-bounds) documentation.

### The `stack upload --tar-dir` option

The `--tar-dir <path_to_directory>` option determines whether the package
archive should be copied to the specified directory.

### The `stack upload --[no-]test-tarball` flag

Default: Disabled

Set the flag to cause Stack to test the resulting package archive, by attempting
to build it.

## Docker integration

Stack is able to build your code inside a Docker image, which means even more
reproducibility to your builds, since you and the rest of your team will always
have the same system libraries.

For further information, see the [Docker integration](docker_integration.md)
documentation.

## Nix integration

Stack provides an integration with [Nix](http://nixos.org/nix), providing you
with the same two benefits as the first Docker integration discussed above:

* more reproducible builds, since fixed versions of any system libraries and
  commands required to build the project are automatically built using Nix and
  managed locally per-project. These system packages never conflict with any
  existing versions of these libraries on your system. That they are managed
  locally to the project means that you don't need to alter your system in any
  way to build any odd project pulled from the Internet.
* implicit sharing of system packages between projects, so you don't have more
  copies on-disk than you need to.

When using the Nix integration, Stack downloads and builds Haskell dependencies
as usual, but resorts on Nix to provide non-Haskell dependencies that exist in
the Nixpkgs.

Both Docker and Nix are methods to *isolate* builds and thereby make them more
reproducible. They just differ in the means of achieving this isolation. Nix
provides slightly weaker isolation guarantees than Docker, but is more
lightweight and more portable (Linux and macOS mainly, but also Windows). For
more on Nix, its command-line interface and its package description language,
read the [Nix manual](http://nixos.org/nix/manual). But keep in mind that the
point of Stack's support is to obviate the need to write any Nix code in the
common case or even to learn how to use the Nix tools (they're called under the
hood).

For more information, see the [Nix-integration](nix_integration.md)
documentation.

## Continuous integration (CI)

### GitHub Actions

The Stack repository uses GitHub Actions for its own CI. For further
information, see the guide to
[contributing](CONTRIBUTING.md#continuous-integration-ci).

### Azure

For further information, see the [Azure CI](azure_ci.md) documentation.

### Travis

For further information, see the [Travis CI](travis_ci.md) documentation.

## Editor integration

### The `intero` project

For editor integration, Stack has a related project called
[intero](https://github.com/commercialhaskell/intero). It is particularly well
supported by Emacs, but some other editors have integration for it as well.

### Shell auto-completion

Love tab-completion of commands? You're not alone. If you're on bash, just run
the following command (or add it to `.bashrc`):

~~~text
eval "$(stack --bash-completion-script stack)"
~~~

For more information and other shells, see the
[Shell auto-completion wiki page](https://docs.haskellstack.org/en/stable/shell_autocompletion)

## Debugging

To profile a component of the current project, simply pass the `--profile`
flag to `stack`. The `--profile` flag turns on the `--enable-library-profiling`
and `--enable-executable-profiling` Cabal options _and_ passes the `+RTS -p`
runtime options to any testsuites and benchmarks.

For example the following command will build the `my-tests` testsuite with
profiling options and create a `my-tests.prof` file in the current directory
as a result of the test run.

~~~text
stack test --profile my-tests
~~~

The `my-tests.prof` file now contains time and allocation info for the test run.

To create a profiling report for an executable, e.g. `my-exe`, you can command:

~~~text
stack exec --profile -- my-exe +RTS -p
~~~

For more fine-grained control of compilation options there are the
`--library-profiling` and `--executable-profiling` flags which will turn on the
`--enable-library-profiling` and `--enable-executable-profiling` Cabal
options respectively. Custom GHC options can be passed in with
`--ghc-options "more options here"`.

To enable compilation with profiling options by default you can add the
following snippet to your `stack.yaml` or `~/.stack/config.yaml`:

~~~yaml
build:
  library-profiling: true
  executable-profiling: true
~~~

### Further reading

For more commands and uses, see the
[official GHC chapter on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html),
the [Haskell wiki](https://wiki.haskell.org/How_to_profile_a_Haskell_program),
and the
[chapter on profiling in Real World Haskell](http://book.realworldhaskell.org/read/profiling-and-optimization.html).

### Tracing

To generate a backtrace in case of exceptions during a test or benchmarks run,
use the `--trace` flag. Like `--profile` this compiles with profiling options,
but adds the `+RTS -xc` runtime option.

### Debugging symbols

Building with debugging symbols in the
[DWARF information](https://ghc.haskell.org/trac/ghc/wiki/DWARF) is supported by
Stack. This can be done by passing the flag `--ghc-options="-g"` and also to
override the default behaviour of stripping executables of debugging symbols by
passing either one of the following flags: `--no-strip`,
`--no-library-stripping` or `--no-executable-stripping`.

In Windows, GDB can be installed to debug an executable with
`stack exec -- pacman -S gdb`. Windows' Visual Studio compiler's debugging
format PDB is not supported at the moment. This might be possible by
[separating](https://stackoverflow.com/questions/866721/how-to-generate-gcc-debug-symbol-outside-the-build-target)
debugging symbols and
[converting](https://github.com/rainers/cv2pdb) their format. Or as an option
when
[using the LLVM backend](http://blog.llvm.org/2017/08/llvm-on-windows-now-supports-pdb-debug.html).
