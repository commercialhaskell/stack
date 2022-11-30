<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack ide` commands

~~~text
stack ide COMMAND

Available commands:
  packages                 List all available local loadable packages
  targets                  List all available Stack targets
~~~

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
