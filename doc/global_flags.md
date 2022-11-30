<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack's global flags and options

Stack can also be configured by flags and options on the command line. Global
flags and options apply to all of Stack's commands. In addition, all of Stack's
commands accept the `--setup-info-yaml` and `--snapshot-location-base` options
and the `--help` flag.

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

## The `--setup-info-yaml` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml`

The `--setup-info-yaml <url>` command option specifies the location of a `setup-info` dictionary. The option can be specified multiple times.

## The `--snapshot-location-base` command option

Default: `https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master`

The `--snapshot-location-base <url>` command option specifies the base location of snapshots.

## The `--help` command flag

If Stack is passed the `--help` command flag, it will output help for the command.
