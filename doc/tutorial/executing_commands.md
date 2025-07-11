  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 12. Executing commands

We have already used `stack exec` multiple times in this guide. As you have
likely already guessed, it allows you to run executables, but with a slightly 
modified environment.

## The `stack exec` command

In particular: `stack exec` looks for executables on Stack's bin
paths, and sets a few additional environment variables (like adding those paths
to the PATH, and setting `GHC_PACKAGE_PATH`, which tells GHC which package
databases to use).

If you want to see exactly what the modified environment looks like, try
command:

~~~text
stack exec env
~~~

The only issue is how to distinguish flags to be passed to Stack versus those
for the underlying program. Thanks to the `optparse-applicative` library, Stack
follows the Unix convention of `--` to separate these. For example, command:

~~~text
stack exec --package stm -- echo I installed the stm package via --package stm
~~~

yields output like:

~~~text
Writing the configuration file for the implicit global project to:
.../global-project/stack.yaml. Note: You can change the snapshot via the
snapshot key there.
Using the latest snapshot lts-22.31.
I installed the stm package via --package stm
~~~

Flags worth mentioning:

* `--package foo` can be used to force a package to be installed before running
  the given command.
* `--no-ghc-package-path` can be used to stop the `GHC_PACKAGE_PATH` environment
  variable from being set. Some tools — notably Cabal (the tool) — do not behave
  well with that variable set.

You may also find it convenient to use `stack exec` to launch a subshell
(substitute `bash` with your preferred shell) where your compiled executable is
available at the front of your PATH. Command:

~~~text
stack exec bash
~~~

## The `stack ghc` and `stack runghc` commands

You will sometimes want to just compile (or run) a single Haskell source file,
instead of creating an entire Cabal package for it. You can use `stack exec ghc`
or `stack exec runghc` for that. As simple helpers, we also provide the
`stack ghc` and `stack runghc` commands, for these common cases.
