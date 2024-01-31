<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack's global flags and options

Stack can also be configured by flags and options on the command line. Global
flags and options apply to all of Stack's commands. In addition, all of Stack's
commands accept the `--setup-info-yaml` and `--snapshot-location-base` options
and the `--help` flag.

## `--allow-different-user` flag

Restrictions: POSIX systems only

Default: True, if inside Docker; false otherwise

Enable/disable permitting users other than the owner of the
[Stack root](stack_root.md) directory to use a Stack installation. For further
information, see the documentation for the corresponding non-project specific
configuration [option](yaml_configuration.md#allow-different-user).

## `--arch` option

Pass the option `--arch <architecture>` to specify the relevant machine
architecture. For further information, see the documentation for the
corresponding non-project specific configuration
[option](yaml_configuration.md#arch).

## `--color` or `-colour` options

Pass the option `stack --color <when>` to specify when to use color in output.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#color).

## `--compiler` option

Pass the option `--compiler <compiler>` to specify the compiler. For further
information, see the [YAML configuration](yaml_configuration.md#compiler)
documentation.

## `--custom-preprocessor-extensions` option

Pass the option `--custom-preprocessor-extensions <extension>` to specify an
extension used for a custom preprocessor. For further information, see the
documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#custom-preprocessor-extensions).

## `--docker*` flags and options

Stack supports automatically performing builds inside a Docker container. For
further information see `stack --docker-help` or the
[Docker integratiom](docker_integration.md) documentation.

## `--[no-]dump-logs` flag

Default: Dump warning logs

Enables/disables the dumping of the build output logs for local packages to the
console. For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#dump-logs).

## `--extra-include-dirs` option

Pass the option `--extra-include-dirs <director>` to specify an extra directory
to check for C header files. The option can be specified multiple times. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#extra-include-dirs).

## `--extra-lib-dirs` option

Pass the option `--extra-lib-dirs <director>` to specify an extra directory
to check for libraries. The option can be specified multiple times. For further
information, see the documentation for the corresponding non-project specific
configuration [option](yaml_configuration.md#extra-lib-dirs).

## `--ghc-build` option

Pass the option `--ghc-build <build>` to specify the relevant specialised GHC
build. For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#ghc-build).

## `--ghc-variant` option

Pass the option `--ghc-variant <variant>` to specify the relevant GHC variant.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#ghc-variant).

## `--hpack-numeric-version` flag

Pass the flag `--hpack-numeric-version` to cause Stack to report the numeric
version of its built-in Hpack library to the standard output stream (e.g.
`0.35.0`) and quit.

## `--[no-]install-ghc` flag

Default: Enabled

Enables/disables the download and instalation of GHC if necessary. For further
information, see the documentation for the corresponding non-project specific
configuration [option](yaml_configuration.md#install-ghc).

## `--jobs` or `-j` option

Pass the option `--jobs <number_of_jobs>` to specify the number of concurrent
jobs (Stack actions during building) to run.

When [building GHC from source](yaml_configuration.md#building-ghc-from-source),
specifies the `-j[<n>]` flag of GHC's Hadrian build system.

By default, Stack specifies a number of concurrent jobs equal to the number of
CPUs (cores) that the machine has. In some circumstances, that default can cause
some machines to run out of memory during building. If those circumstances
arise, specify `--jobs 1`.

This configuration option is distinct from GHC's own `-j[<n>]` flag, which
relates to parallel compilation of modules within a package.

For further information, see the documentation for the corresponding non-project
specific configuration option: [`jobs`](yaml_configuration.md#jobs).

## `--local-bin-path` option

Pass the option `--local-bin-path <directory>` to set the target directory for
[`stack build --copy-bins`](build_command.md#-no-copy-bins-flag) and
`stack install`. An absolute or relative path can be specified. A relative path
at the command line is always assumed to be relative to the current directory.

For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#local-bin-path).

## `--lock-file` option

Default: `read-write`, if snapshot specified in YAML configuration file;
`read-only`, if a different snapshot is specified on the command line.

Pass the option `--lock-file <mode>` to specify how Stack interacts with lock
files. Valid modes are:

* `error-on-write`: Stack reports an error, rather than write a lock file;
* `ignore`: Stack ignores lock files;
* `read-only`: Stack only reads lock files; and
* `read-write`: Stack reads and writes lock files.

## `--[no-]modify-code-page` flag

Restrictions: Windows systems only

Default: Enabled

Enables/disables setting the codepage to support UTF-8. For further information,
see the documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#modify-code-page).

## `--nix*` flags and options

Stack can be configured to integrate with Nix. For further information, see
`stack --nix-help` or the [Nix integration](nix_integration.md) documentation.

## `--numeric-version` flag

Pass the flag `--numeric-version` to cause Stack to report its numeric version
to the standard output stream (e.g. `2.9.1`) and quit.

## `--[no-]plan-in-log` flag

[:octicons-tag-24: 2.13.1](https://github.com/commercialhaskell/stack/releases/tag/v2.13.1)

Default: Disabled

Enables/disables the logging of build plan construction in debug output.
Information about the build plan construction can be lengthy. If you do not need
it, it is best omitted from the debug output.

## `--resolver` option

Pass the option `--resolver <snapshot>` to specify the snapshot. For further
information, see the
[YAML configuration](yaml_configuration.md#snapshot) documentation.

At the command line (only):

*   `--resolver lts-<major_version>` specifies the latest Stackage LTS Haskell
    snapshot with the specified major version;
*   `--resolver lts` specifies, from those with the greatest major version, the
    latest Stackage LTS Haskell snapshot; and
*   `--resolver nightly` specifies the most recent Stackage Nightly snapshot.

## `--[no-]rsl-in-log` flag

[:octicons-tag-24: 2.9.1](https://github.com/commercialhaskell/stack/releases/tag/v2.9.1)

Default: Disabled

Enables/disables the logging of the raw snapshot layer (rsl) in debug output.
Information about the raw snapshot layer can be lengthy. If you do not need it,
it is best omitted from the debug output.

## `--[no-]script-no-run-compile` flag

Default: Disabled

Enables/disables the use of options `--no-run --compile` with the
[`stack script` command](script_command.md).

## `--silent` flag

Equivalent to the `--verbosity silent` option.

## `--[no-]skip-ghc-check` option

Default: Disabled

Enables/disables the skipping of checking the GHC version and architecture. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#skip-ghc-check).

## `--[no-]skip-msys` option

Restrictions: Windows systems only

Default: Disabled

Enables/disables the skipping of installing MSYS2. For further information, see
the documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#skip-msys).

## `--stack-colors` or `--stack-colours` options

Pass the option `--stack-colors <styles>` to specify Stack's output styles. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#stack-colors).

## `--stack-root` option

Overrides: `STACK_ROOT` environment variable

Pass the option `--stack-root <absolute_path_to_the_Stack_root>` to specify the
path to the [Stack root](stack_root.md) directory. The path must be an absolute
one.

## `--stack-yaml` option

Default: `stack.yaml`

Overrides: `STACK_YAML` enviroment variable

Pass the option `--stack-yaml <file>` to specify Stack's project-level YAML
configuration file.

## `--[no-]system-ghc` flag

Default: Disabled

Enables/disables the use of a GHC executable on the PATH, if one is available
and its version matches.

## `--[no-]terminal` flag

Default: Stack is running in a terminal (as detected)

Enables/disables whether Stack is running in a terminal.

## `--terminal-width` option

Default: the terminal width (if detected); otherwise `100`

Pass the option `--terminal-width <width>` to specify the width of the terminal,
used by Stack's pretty printed messages.

## `--[no-]time-in-logs` flag

Default: Enabled

Enables/disables the inclusion of time stamps against logging entries when the
verbosity level is 'debug'.

## `--verbose` or `-v` flags

Equivalent to the `--verbosity debug` option.

## `--verbosity` option

Default: `info`

Pass the option `--verbosity <log_level>` to specify the level for logging.
Possible levels are `silent`, `error`, `warn`, `info` and `debug`, in order of
increasing amounts of information provided by logging.

## `--version` flag

Pass the flag `--version` to cause Stack to report its version to standard
output and quit. For versions that are release candidates, the report will list
the dependencies that Stack has been compiled with.

## `--with-gcc` option

Pass the option `--with-gcc <path_to_gcc>` to specify use of a GCC executable.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#with-gcc).

## `--with-hpack` option

Pass the option `--with-hpack <hpack>` to specify use of an Hpack executable.
For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#with-hpack).

## `--work-dir` option

Default: `.stack-work`

Overrides: [`STACK_WORK`](environment_variables.md#stack_work) environment
variable, and [`work-dir`](yaml_configuration.md) non-project specific
configuration option.

Pass the option `--work-dir <relative_path_to_the_Stack_root>` to specify the
path to Stack's work directory, within a local project or package directory. The
path must be a relative one, relative to the the root directory of the project
or package. The relative path cannot include a `..` (parent directory)
component.

## `--setup-info-yaml` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml`

The `--setup-info-yaml <url>` command option specifies the location of a
`setup-info` dictionary. The option can be specified multiple times.

## `--snapshot-location-base` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master`

The `--snapshot-location-base <url>` command option specifies the base location
of snapshots.

## `--help` command flag

If Stack is passed the `--help` command flag, it will output help for the
command.
