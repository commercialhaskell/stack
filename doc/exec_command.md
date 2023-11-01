<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack exec` command

~~~text
stack exec COMMAND
           [-- ARGUMENT(S) (e.g. stack exec ghc-pkg -- describe base)]
           [--[no-]ghc-package-path] [--[no-]stack-exe] [--package PACKAGE]
           [--rts-options RTSFLAG] [--cwd DIR]
~~~

`stack exec` executes the specified executable as a command in the Stack
environment. If an executable is not specified, the first argument after `--` is
taken to be the executable. Otherwise, all arguments after `--` are taken to be
command line arguments for the specified executable.

By default:

* the `GHC_PACKAGE_PATH` environment variable is set for the command's process.
  Pass the flag `--no-ghc-package-path` to not set the environment variable;

* the `STACK_EXE` environment variable is set for the command's process. Pass
  the flag `--no-stack-exe` to not set the environment variable; and

* the specified executable is executed in the current directory. Pass the option
  `--cwd <directory>` to execute the executable in the specified directory.

The option `--package <package>` has no effect for the `stack exec` command. For
further information about its use, see the [`stack ghc` command](ghc_command.md)
documentation or the [`stack runghc` command](runghc_command.md) documentation.

Pass the option `--rts-option <rts_flag(s)>` to specify a GHC RTS flag or option.
The option can be specified multiple times. All specified GHC RTS flags and
options are added to the arguments for the specified executable between
arguments `+RTS` and `-RTS`.

Specified GHC RTS flags and options are separated by spaces. Items can be
unquoted (if they do not contain space or `"` characters) or quoted (`""`).
Quoted items can include 'escaped' characters, escaped with an initial `\`
character.
