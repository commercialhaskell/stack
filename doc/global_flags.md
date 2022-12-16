<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack's global flags and options

Stack can also be configured by flags and options on the command line. Global
flags and options apply to all of Stack's commands. In addition, all of Stack's
commands accept the `--setup-info-yaml` and `--snapshot-location-base` options
and the `--help` flag.

## The `--allow-different-user` flag

Restrictions: POSIX systems only

Default: True, if inside Docker; false otherwise

Enable/disable permitting users other than the owner of the Stack root directory
to use a Stack installation. For further information, see the documentation for
the corresponding non-project specific configuration
[option](yaml_configuration.md#allow-different-user).

## The `--arch` option

Pass the option `--arch <architecture>` to specify the relevant machine
architecture. For further information, see the documentation for the
corresponding non-project specific configuration
[option](yaml_configuration.md#arch).

## The `--color` or `-colour` options

Pass the option `stack --color <when>` to specify when to use color in output.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#color).

## The `--compiler` option

Pass the option `--compiler <compiler>` to specify the compiler. For further
information, see the [YAML configuration](yaml_configuration.md#compiler)
documentation.

## The `--custom-preprocessor-extensions` option

Pass the option `--custom-preprocessor-extensions <extension>` to specify an
extension used for a custom preprocessor. For further information, see the
documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#custom-preprocessor-extensions).

## The `--docker*` flags and options

Stack supports automatically performing builds inside a Docker container. For
further information see `stack --docker-help` or the
[Docker integratiom](docker_integration.md) documentation.

## The `--[no-]dump-logs` flag

Default: Dump warning logs

Enables/disables the dumping of the build output logs for local packages to the
console. For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#dump-logs).

## The `--extra-include-dirs` option

Pass the option `--extra-include-dirs <director>` to specify an extra directory
to check for C header files. The option can be specified multiple times. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#extra-include-dirs).

## The `--extra-lib-dirs` option

Pass the option `--extra-lib-dirs <director>` to specify an extra directory
to check for libraries. The option can be specified multiple times. For further
information, see the documentation for the corresponding non-project specific
configuration [option](yaml_configuration.md#extra-lib-dirs).

## The `--ghc-build` option

Pass the option `--ghc-build <build>` to specify the relevant specialised GHC
build. For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#ghc-build).

## The `--ghc-variant` option

Pass the option `--ghc-variant <variant>` to specify the relevant GHC variant.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#ghc-variant).

## The `--hpack-numeric-version` flag

Pass the flag `--hpack-numeric-version` to cause Stack to report the numeric
version of its built-in Hpack library to standard output (e.g. `0.35.0`) and
quit.

## The `--[no-]install-ghc` flag

Default: Enabled

Enables/disables the download and instalation of GHC if necessary. For further
information, see the documentation for the corresponding non-project specific
configuration [option](yaml_configuration.md#install-ghc).

## The `--jobs` or `-j` option

Pass the option `--jobs <number_of_jobs>` to specify the number of concurrent
jobs to run. For further information, see the documentation for the
corresponding non-project specific configuration
[option](yaml_configuration.md#jobs).

## The `--local-bin-path` option

Pass the option `--local-bin-path <directory>` to specify the directory in which
Stack installs executables. For further information, see the documentation for
the corresponding non-project specific configuration
[option](yaml_configuration.md#local-bin-path).

## The `--lock-file` option

Default: `read-write`, if snapshot specified in YAML configuration file;
`read-only`, if a different snapshot is specified on the command line.

Pass the option `--lock-file <mode>` to specify how Stack interacts with lock
files. Valid modes are `error-on-write`, `ignore`, `read-only` and `read-write`.

## The `--[no-]modify-code-page` flag

Restrictions: Windows systems only

Default: Enabled

Enables/disables setting the codepage to support UTF-8. For further information,
see the documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#modify-code-page).

## The `--nix*` flags and options

Stack can be configured to integrate with Nix. For further information, see
`stack --nix-help` or the [Nix integration](nix_integration.md) documentation.

## The `--numeric-version` flag

Pass the flag `--numeric-version` to cause Stack to report its numeric version
to standard output (e.g. `2.9.1`) and quit.

## The `--resolver` option

Pass the option `--resolver <snapshot>` to specify the snapshot. For further
information, see the
[YAML configuration](yaml_configuration.md#resolver-or-snapshot) documentation.

## The `--[no-]rsl-in-log` flag

[:octicons-tag-24: 2.9.1](https://github.com/commercialhaskell/stack/releases/tag/v2.9.1)

Default: Disabled

Enables/disables the logging of the raw snapshot layer (rsl) in debug output.
Information about the raw snapshot layer can be lengthy. If you do not need it,
it is best omitted from the debug output.

## The `--[no-]script-no-run-compile` flag

Default: Disabled

Enables/disables the use of options `--no-run --compile` with the
[`stack script` command](script_command.md).

## The `--silent` flag

Equivalent to the `--verbosity silent` option.

## The `--[no-]skip-ghc-check` option

Default: Disabled

Enables/disables the skipping of checking the GHC version and architecture. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#skip-ghc-check).

## The `--[no-]skip-msys` option

Restrictions: Windows systems only

Default: Disabled

Enables/disables the skipping of installing MSYS2. For further information, see
the documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#skip-msys).

## The `--stack-colors` or `--stack-colours` options

Pass the option `--stack-colors <styles>` to specify Stack's output styles. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#stack-colors).

## The `--stack-root` option

Overrides: `STACK_ROOT` environment variable

Pass the option `--stack-root <absolute_path_to_the_Stack_root>` to specify the
path to the Stack root directory. The path must be an absolute one.

## The `--stack-yaml` option

Default: `stack.yaml`

Overrides: `STACK_YAML` enviroment variable

Pass the option `--stack-yaml <file>` to specify Stack's project-level YAML
configuration file.

## The `--[no-]system-ghc` flag

Default: Disabled

Enables/disables the use of a GHC executable on the PATH, if one is available
and its version matches.

## The `--[no-]terminal` flag

Default: Stack is running in a terminal (as detected)

Enables/disables whether Stack is running in a terminal.

## The `--terminal-width` option

Default: the terminal width (if detected); otherwise `100`

Pass the option `--terminal-width <width>` to specify the width of the terminal,
used by Stack's pretty printed messages.

## The `--[no-]time-in-logs` flag

Default: Enabled

Enables/disables the inclusion of time stamps against logging entries when the
verbosity level is 'debug'.

## The `--verbose` or `-v` flags

Equivalent to the `--verbosity debug` option.

## The `--verbosity` option

Default: `info`

Pass the option `--verbosity <log_level>` to specify the level for logging.
Possible levels are `silent`, `error`, `warn`, `info` and `debug`, in order of
increasing amounts of information provided by logging.

## The `--version` flag

Pass the flag `--version` to cause Stack to report its version to standard
output and quit. For versions that are release candidates, the report will list
the dependencies that Stack has been compiled with.

## The `--with-gcc` option

Pass the option `--with-gcc <path_to_gcc>` to specify use of a GCC executable.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#with-gcc).

## The `--with-hpack` option

Pass the option `--with-hpack <hpack>` to specify use of an Hpack executable.
For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#with-hpack).

## The `--work-dir` option

Default: `.stack-work`

Overrides: `STACK_WORK` environment variable

Pass the option `--work-dir <relative_path_to_the_Stack_root>` to specify the
path to Stack's work directory for the project. The path must be a relative one,
relative to the project's root directory. For further information, see the
documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#work-dir).

## The `--setup-info-yaml` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml`

The `--setup-info-yaml <url>` command option specifies the location of a
`setup-info` dictionary. The option can be specified multiple times.

## The `--snapshot-location-base` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master`

The `--snapshot-location-base <url>` command option specifies the base location
of snapshots.

## The `--help` command flag

If Stack is passed the `--help` command flag, it will output help for the
command.
