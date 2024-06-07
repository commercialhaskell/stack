<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack script` command

~~~text
stack script [--package PACKAGE] FILE
             [-- ARGUMENT(S) (e.g. stack script X.hs -- argument(s) to program)]
             [--compile | --optimize] [--[no-]use-root] [--ghc-options OPTIONS]
             [--extra-dep PACKAGE-VERSION] [--no-run]
~~~

The `stack script` command either runs a specified Haskell source file (using
GHC's `runghc`) or, optionally, compiles such a file (using GHC) and, by
default, runs it.

Unlike [`stack ghc`](ghc_command.md) and [`stack runghc`](runghc_command.md),
the command ignores any project-level configuration file (`stack.yaml`, by
default) (including in the `global-project` directory in the Stack root). A
snapshot must be specified on the command line (with the `--snapshot` option).
For example:

~~~text
stack script --snapshot lts-22.21 MyScript.hs
~~~

!!! info

    Non-project level configuration options in global configuration files
    (`config.yaml`), are not ignored. Such options may be useful if
    [`allow-newer`](yaml_configuration.md#allow-newer) and/or
    [`allow-newer-deps`](yaml_configuration.md#allow-newer-deps) are required.

The `stack script` command behaves as if the `--install-ghc` flag had been
passed at the command line.

Everything after `--` on the command line is interpreted as a command line
argument to be passed to what is run.

A package can be added to the snapshot on the command line with the
`--extra-dep` option (which can be specified multiple times).

Each required package can be specified by name on the command line with the
`--package` option (which can be specified multiple times). A single `--package`
option can also refer to a list of package names, separated by a space or comma
character. If the package is not in the snapshot, the most recent version in the
package index (e.g. Hackage) will be obtained.

If no packages are specified in that way, all the required packages that are in
the snapshot or are a GHC boot package (packages that come with GHC and are
included in GHC's global package database) will be deduced by reference to the
`import` statements in the source file. The `base` package associated with the
version of GHC specified by the snapshot is always available.

If a required package is a GHC boot package, the behaviour can be complex. If
the boot package has not been 'replaced', then it will be used in Stack's build
plan. However, if the boot package has been 'replaced', the latest version of
that package in the package index will be used in Stack's build plan, which may
differ from the version provided by the version of GHC specified by the
snapshot. A boot package will be treated as 'replaced' if the package i
included directly in the Stackage snapshot or it depends on a package included
directly in the snapshot. Stackage snapshots do not include directly most boot
packages but some snapshots may include directly some boot packages. In
particular, some snapshots include directly `Win32` (which is a boot package on
Windows) while others do not. For example, if `Cabal` (a boot package) is a
required package then, with Stackage snapshot LTS Haskell 20.25, Stack will:

* on Windows, try to construct a build plan based on the latest version of
  `Cabal` in the package index (because that snapshot includes `Win32` directly,
  and `Cabal` depends on `Win32` and so is treated as 'replaced'); and
* on non-Windows, use the boot package in the build plan (because `Cabal` is not
  'replaced').

Boot packages that have been 'replaced' can be specified as an `--extra-dep`.

The source file can be compiled by passing either the `--compile` flag (no
optimization) or the `--optimize` flag (compilation with optimization). If the
file is compiled, passing the `--no-run` flag will mean the compiled code is not
run.

By default, all the compilation outputs (including the executable) are written
to the directory of the source file. Pass the `--use-root` flag to write such
outputs to a script-specific location in the `scripts` directory of the Stack
root. The location reflects the absolute path to the source file, but ignoring
the drive. This can avoid clutter in the source file directory.

Additional options can be passed to GHC using the `--ghc-options` option.

## Examples

For example, Haskell source file `MyScript.hs` at location
`<drive>Users/jane/my-project` (where `<drive>` could be `/` on Unix-like
operating systems or `C:/` or similar on Windows):

~~~haskell
module Main (main) where

import Data.List (intercalate)
import System.Environment (getArgs)

import Acme.Missiles (launchMissiles)

main :: IO ()
main = do
  advices <- getArgs
  launchMissiles
  putStrLn $ intercalate "\n" advices
~~~

can be compiled and run, with arguments, with:

~~~text
stack --snapshot lts-22.21 script --package acme-missiles --compile MyScript.hs -- "Don't panic!" "Duck and cover!"
~~~

All the compilation outputs (like `Main.hi`, `Main.o`, and the executable
`MyScript`) will be written to the `my-project` directory.

If compiled and run with the additional flag `--use-root`, all the compilation
outputs will be written to a directory named `MyScript.hs` at
`Users/jane/my-project/` in the `scripts` directory of the Stack root.

For example, consider the following script extract, based on snapshot Stackage
LTS Haskell 20.25, where considerations on Windows differ from non-Windows. The
`stack script` command is specified using Stack's
[script interpreter](scripts.md).

=== "Windows"

    The snapshot includes `Win32` directly. As a consequence, GHC boot packages
    `directory`, `process` and `time` (which depend on `Win32`) are all treated
    as 'replaced'.

    ~~~haskell
    {- stack script
       --snapshot lts-20.25
       --extra-dep acme-missiles-0.3
       --extra-dep directory-1.3.6.2
       --extra-dep process-1.6.16.0
       --extra-dep time-1.11.1.1
    -}

    import Acme.Missiles -- from acme-missiles
    import Data.Time.Clock.System -- from time
    import System.Time.Extra -- from extra

    ...
    ~~~

    `acme-missiles` is not in the snapshot and so needs to be specified as an
    extra-dep.

    Stack can deduce that the module imports imply that the required packages
    are `acme-missiles`, `time` and `extra` (which is in the snapshot).

    `extra` depends on `directory` and `process`. If `directory` and `process`
    are not specified as extra-deps, Stack will complain that they have been
    'pruned'.

    `directory-1.3.6.2` depends on `time < 1.12`. If `time` is not specified as
    an extra-dep, Stack will try to construct a build plan based on the latest
    version in the package index (which will fail, as the latest version is
    `>= 1.12`)

=== "Unix-like"

    ~~~haskell
    {- stack script
       --snapshot lts-20.25
       --extra-dep acme-missiles-0.3
    -}

    import Acme.Missiles -- from acme-missiles
    import Data.Time.Clock.System -- from time
    import System.Time.Extra -- from extra

    ...
    ~~~

    `acme-missiles` is not in the snapshot and so needs to be specified as an
    extra-dep.

    Stack can deduce that the module imports imply that the required packages
    are `acme-missiles`, `time` and `extra` (which is in the snapshot).

    All the other dependencies required are either GHC boot packages (which have
    not been 'replaced') or in the snapshot.
