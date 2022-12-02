<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack's global flags and options

Stack can also be configured by flags and options on the command line. Global
flags and options apply to all of Stack's commands. In addition, all of Stack's
commands accept the `--setup-info-yaml` and `--snapshot-location-base` options
and the `--help` flag.

## The `stack --allow-different-user` flag

Restrictions: POSIX systems only

Default: True, if inside Docker; false otherwise

Enable/disable permitting users other than the owner of the Stack root directory
to use a Stack installation. For further information, see the documentation for
the corresponding non-project specific configuration
[option](yaml_configuration.md#allow-different-user).

## The `stack --arch` option

Pass the option `--arch <architecture>` to specify the relevant machine
architecture. For further information, see the documentation for the
corresponding non-project specific configuration
[option](yaml_configuration.md#arch).

## The `stack --color` or `-colour` options

Pass the option `stack --color <when>` to specify when to use color in output.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#color).

## The `stack --compiler` option

Pass the option `--compiler <compiler>` to specify the compiler. For further
information, see the [YAML configuration](yaml_configuration.md#compiler)
documentation.

## The `stack --custom-preprocessor-extensions` option

Pass the option `--custom-preprocessor-extensions <extension>` to specify an
extension used for a custom preprocessor. For further information, see the
documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#custom-preprocessor-extensions).

## The `stack --docker*` flags and options

Stack supports automatically performing builds inside a Docker container. For
further information see `stack --docker-help` or the
[Docker integratiom](docker_integration.md) documentation.

## The `stack --[no-]dump-logs` flag

Default: Dump warning logs

Enables/disables the dumping of the build output logs for local packages to the
console. For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#dump-logs).

## The `stack --extra-include-dirs` option

Pass the option `--extra-include-dirs <director>` to specify an extra directory
to check for C header files.  For further information, see the documentation for
the corresponding non-project specific configuration
[option](yaml_configuration.md#extra-include-dirs).

## The `stack --extra-lib-dirs` option

Pass the option `--extra-lib-dirs <director>` to specify an extra directory
to check for libraries. For further information, see the documentation for
the corresponding non-project specific configuration
[option](yaml_configuration.md#extra-lib-dirs).

## The `stack --ghc-build` option

Pass the option `--ghc-build <build>` to specify the relevant specialised GHC
build. For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#ghc-build).

## The `stack --ghc-variant` option

Pass the option `--ghc-variant <variant>` to specify the relevant GHC variant.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#ghc-variant).

## The `stack --hpack-numeric-version` flag

Stack will report the numeric version of its built-in Hpack library to standard
output (e.g. `0.35.0`) and quit.

## The `stack --[no-]install-ghc` flag

Default: Enabled

Enables/disables the download and instalation of GHC if necessary. For further
information, see the documentation for the corresponding non-project specific
configuration [option](yaml_configuration.md#install-ghc).

## The `stack --jobs` or `-j` option

Pass the option `--jobs <number_of_jobs>` to specify the number of concurrent
jobs to run. For further information, see the documentation for the
corresponding non-project specific configuration
[option](yaml_configuration.md#jobs).

## The `stack --local-bin-path` option

Pass the option `--local-bin-path <directory>` to specify the directory in which
Stack installs executables. For further information, see the documentation for
the corresponding non-project specific configuration
[option](yaml_configuration.md#local-bin-path).

## The `stack --lock-file` option

Default: `read-write`, if snapshot specified in YAML configuration file;
`read-only`, if a different snapshot is specified on the command line.

Pass the option `--lock-file <mode>` to specify how Stack interacts with lock
files. Valid modes are `error-on-write`, `ignore`, `read-only` and `read-write`.

## The `stack --[no-]modify-code-page` flag

Restrictions: Winfows systems only

Default: Enabled

Enables/disables setting the codepage to support UTF-8. For further information,
see the documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#modify-code-page).

## The `stack --nix*` flags and options

Stack can be configured to integrate with Nix. For further information, see
`stack --nix-help` or the [Nix integration](nix_integration.md) documentation.

## The `stack --numeric-version` flag

Stack will report its numeric version to standard output (e.g. `2.9.1`) and
quit.

## The `stack --resolver` option

Pass option `--resolver <snapshot>` to specify the snapshot. For further
information, see the
[YAML configuration](yaml_configuration.md#resolver-or-snapshot) documentation.

## The `stack --[no-]rsl-in-log` flag

[:octicons-tag-24: 2.9.1](https://github.com/commercialhaskell/stack/releases/tag/v2.9.1)

Default: Disabled

Enables/disables the logging of the raw snapshot layer (rsl) in debug output.
Information about the raw snapshot layer can be lengthy. If you do not need it,
it is best omitted from the debug output.

## The `stack --[no-]script-no-run-compile` flag

Default: Disabled

Enables/disables the use of options `--no-run --compile` with the
[`stack script` command](script_command.md).

## The `stack --silent` flag

Equivalent to the `stack --verbosity silent` option.

## The `stack --[no-]skip-ghc-check` option

Default: Disabled

Enables/disables the skipping of checking the GHC version and architecture. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#skip-ghc-check).

## The `stack --[no-]skip-msys` option

Restrictions: Windows systems only

Default: Disabled

Enables/disables the skipping of installing MSYS2. For further information, see
the documentation for the corresponding non-project specific configuration
[option](yaml_configuration.md#skip-msys).

## The `stack --stack-colors` or `--stack-colours` options

Pass the option `--stack-colors <styles>` to specify Stack's output styles. For
further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#stack-colors).

## The `stack --stack-root` option

`stack --stack-root <absolute_path_to_the_Stack_root>` specifies the path to the
Stack root directory. The path must be an absolute one. The option will override
the contents of any `STACK_ROOT` environment variable.

## The `stack --stack-yaml` option

Overrides: `STACK_YAML` enviroment variable

Pass option `--stack-yaml <file>` to specify Stack's project-level YAML
configuration file.

## The `stack --[no-]system-ghc` flag

Default: Disabled

Enables/disables the use of a GHC executable on the PATH, if one is available
and its version matches.

## The `stack --[no-]terminal` flag

Enables/disables the overriding of terminal detection.

## The `stack --terminal-width` option

Pass the option `--terminal-width <width>` to specify the width of the terminal,
used by Stack's pretty printed messages.

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

## The `stack --with-gcc` option

Pass the option `--with-gcc <path_to_gcc>` to specify use of a GCC executable.
For further information, see the documentation for the corresponding non-project
specific configuration [option](yaml_configuration.md#with-gcc).

## The `stack --with-hpack` option

Pass the option `--with-hpack <hpack>` to specify use of an Hpack executable.
For further information, see the documentation for the corresponding
non-project specific configuration [option](yaml_configuration.md#with-hpack).

## The `stack --work-dir` option

Default: `.stack-work`

`stack --work-dir <relative_path_to_the_Stack_root>` specifies the path to
Stack's work directory for the project. The path must be a relative one,
relative to the project's root directory. The option will override the contents
of any `STACK_WORK` environment variable.

## The `--setup-info-yaml` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml`

The `--setup-info-yaml <url>` command option specifies the location of a `setup-info` dictionary. The option can be specified multiple times.

## The `--snapshot-location-base` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master`

The `--snapshot-location-base <url>` command option specifies the base location of snapshots.

## The `--help` command flag

If Stack is passed the `--help` command flag, it will output help for the command.
