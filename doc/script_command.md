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

Unlike `stack ghc` and `stack runghc`, the command ignores all Stack YAML
configuration files (global and project-level). A snapshot must be specified on
the command line (with the `--resolver` option). For example:

~~~text
stack --resolver lts-20.22 MyScript.hs
~~~

or, equivalently:

~~~text
stack script --resolver lts-20.22 MyScript.hs
~~~

The `stack script` command behaves as if the `--install-ghc` flag had been
passed at the command line.

Everything after `--` on the command line is interpreted as a command line
argument to be passed to what is run.

A package can be added to the snapshot on the command line with the
`--extra-dep` option (which can be specified multiple times).

Each required package can be specified by name on the command line with the
`--package` option (which can be specified multiple times). A single `--package`
option can also refer to a list of package names, separated by a space or comma
character. If the package is not in the snapshot, the most recent version on
Hackage will be obtained. If no packages are specified in that way, all the
required packages that are in the snapshot will be deduced by reference to the
`import` statements in the source file. The `base` package associated with the
version of GHC specified by the snapshot is always available.

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
stack --resolver lts-20.22 script --package acme-missiles --compile MyScript.hs -- "Don't panic!" "Duck and cover!"
~~~

All the compilation outputs (like `Main.hi`, `Main.o`, and the executable
`MyScript`) will be written to the `my-project` directory.

If compiled and run with the additional flag `--use-root`, all the compilation
outputs will be written to a directory named `MyScript.hs` at
`Users/jane/my-project/` in the `scripts` directory of the Stack root.
