<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack run` command

~~~text
stack run [-- ARGUMENT(S) (e.g. stack run -- file.txt)]
          [--[no-]ghc-package-path] [--[no-]stack-exe]
          [--package PACKAGE] [--rts-options RTSFLAG] [--cwd DIR]
~~~

`stack run` builds a project executable and runs it. If the command has a first
argument and it is recognised as an executable target then that is built.
Otherwise, the project's first executable is built. If the project has no
executables Stack reports no executables found as an error.

Everything after `--` on the command line is interpreted as a command line
argument to be passed to what is run, other than a first argument recognised as
an executable target.

By default:

*   the `GHC_PACKAGE_PATH` environment variable is set for the subprocess. Pass
    the `--no-ghc-package-path` flag to not set the variable; and

*   the `STACK_EXE` environment variable is set with the path to Stack. Pass the
    `--no-stack-exe` flag to not set the variable.

The `--cwd` option can be used to set the working directory before the
executable is run.

The `--package` option (which can be specified multiple times) can be used to
add a package name to build targets.

The `--rts-options` option (which can be specified multiple times) can be used
to pass a list of GHC's
[runtime system (RTS) options](https://downloads.haskell.org/~ghc/latest/docs/users_guide/runtime_control.html#)
to the executable when it is run. (The `+RTS` and `-RTS` must not be included.)
