<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack script` command

~~~text
stack script [--package PACKAGE] FILE
             [-- ARGUMENT(S) (e.g. stack script X.hs -- argument(s) to program).]
             [--compile | --optimize] [--[no-]use-root] [--ghc-options OPTIONS]
             [--extra-dep EXTRA-DEP] [--no-run]
~~~

The `stack script` command either runs a specified Haskell source file (using
GHC's `runghc`) or, optionally, compiles such a file (using GHC) and, by
default, runs it.

## Global configuration files

Non-project level configuration options in global configuration files
(`config.yaml`) are not ignored by the `stack script` command.

!!! info

    Non-project level configuration options may be useful if
    [`allow-newer`](../configure/yaml/non-project.md#allow-newer) and/or
    [`allow-newer-deps`](../configure/yaml/non-project.md#allow-newer-deps) are
    required.

## Project-level configuration file

The `stack script` command ignores any project-level configuration file
(`stack.yaml`, by default), including in the `global-project` directory in the
Stack root.

!!! info

    The `stack script` command can be contrasted with the
    [`stack ghc`](ghc_command.md) and [`stack runghc`](runghc_command.md)
    commands, which do not ignore any project-level configuration file.

## GHC

The `stack script` command behaves as if the
[`--install-ghc`](../configure/global_flags.md#-no-install-ghc-flag) flag had
been passed at the command line.

## Snapshot and extra-deps

A snapshot must be specified on the command line, using the `--snapshot` option.
For example:

~~~text
stack script --snapshot lts-24.9 MyScript.hs
~~~

An immutable extra-dep can be added to the snapshot on the command line with the
`--extra-dep` option (which can be specified multiple times).

An extra-dep is specified using a valid YAML value. For further information, see
the [package location](../topics/package_location.md) documentation. Examples
are:

~~~text
--extra-dep acme-missiles-0.3@rev:0
--extra-dep '{git: git@github.com:yesodweb/wai, commit: '2f8a8e1b771829f4a8a77c0111352ce45a14c30f', subdirs: [auto-update, wai]}
--extra-dep acme-missiles-0.3.tar.gz
~~~

Relative paths to local archive files are assumed to be relative to the
directory in which the script file is located.

GHC boot packages that have been 'replaced' (see further below) can be specified
as an `--extra-dep`.

## Required packages

The names of required packages can be either deduced or specified.

The `base` package associated with the version of GHC specified by the snapshot
is always available.

If no packages are specified, all the required packages that are in the snapshot
or are a GHC boot package (packages that come with GHC and are included in GHC's
global package database), will be deduced by reference to the `import`
statements in the source file. In that regard, Stack assumes that:

* a line that begins `import` is an `import` statement;
* `import` may be followed by `qualified` on the same line;
* consistent with GHC's
  [`PackageImports`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html)
  language extension, that if `import` or `import qualified` is followed by
  `"<name>"` on the same line, that `<name>` is the name of a required package;
* otherwise, `import` or `import qualified` is followed by the module name on
  the same line. Stack will not deduce the names of hidden packages from
  module names or the names of blacklisted packages.

!!! note

    The first time that Stack deduces package names from module names can take
    some time. Use the `--verbose` option to understand Stack's progress.

!!! note

    The installed packages of modules exposed by public sub-libraries will not
    be deduced, because those installed packages are hidden.

!!! info

    Certain packages are blacklisted because they expose one or more modules
    with names that are the same as modules exposed by more popular packages.
    The blacklisted packages are `Glob`, `HTF`, `async-dejafu`,
    `binary-ieee754`, `cipher-aes`, `cipher-blowfish`, `cipher-camellia`,
    `cipher-des`, `cipher-rc4`, `control-monad-free`, `courier`, `crypto-api`,
    `crypto-cipher-types`, `crypto-numbers`, `crypto-pubkey`, `crypto-random`,
    `cryptohash`, `cryptohash-conduit`, `cryptohash-md5`, `cryptohash-sha1`,
    `cryptohash-sha256`, `fay-base`, `gl`, `gtk3`, `hashmap`, `hledger-web`,
    `hxt-unicode`, `kawhi`, `language-c`, `log`, `monad-extras`, `monads-tf`,
    `nanospec`, `newtype-generics`, `objective`, `plot-gtk3`, `prompt`,
    `regex-compat-tdfa`, `regex-pcre-builtin`, `rerebase`, `svg-tree` and `zip`.

Alternatively, each required package can be specified by name on the command
line with the `--package` option (which can be specified multiple times). A
single `--package` option can also refer to a list of package names, separated
by a space or comma character. If the package is not in the snapshot, the most
recent version in the package index (e.g. Hackage) will be obtained.

In the case of a named public sub-library of a Cabal package, the required
installed package is specified by the 'munged' package name. For example, for
public sub-library `my-library` of Cabal package `my-package` the munged name of
the installed package is `z-my-library-z-my-package`.

If a required package is a GHC boot package, the behaviour can be complex. If
the boot package has not been 'replaced', then it will be used in Stack's build
plan. However, if the boot package has been 'replaced', the latest version of
that package in the package index will be used in Stack's build plan, which may
differ from the version provided by the version of GHC specified by the
snapshot. A boot package will be treated as 'replaced' if the package is
included directly in the Stackage snapshot or it depends on a package included
directly in the snapshot. Stackage snapshots do not include directly most boot
packages but some snapshots may include directly some boot packages. In
particular, some snapshots include directly `Win32` (which is a boot package on
Windows) while others do not.

!!! info

    GHC has the concept of 'installed packages' (which differ from 'Cabal
    packages') in package databases. An installed package has a name. An
    installed package corresponding to the main (unnamed) library of a Cabal
    package has the same name as the Cabal package. An installed package
    corresponding to a sub-library of a Cabal package has a 'munged' name that
    reflects the name of the Cabal package and the name of the sub-library. An
    installed package corresponding to a sub-library also has a `package-name`,
    which is the name of the Cabal package.

    The `--package` option of `stack script` makes use of GHC's `-package-id`
    option to expose an installed package, rather than its `-package` option.
    The latter option treats `package-name` (if it exists) as if it were also
    the name of the installed package. That means, for a Cabal package with one
    or more sub-libraries, the GHC option `-package=<name>` cannot distinguish
    between (a) the installed package `<name>` corresponding to the main library
    of Cabal package `<name>` and (b) an installed package corresponding to a
    sub-library of that Cabal package. The installed package that GHC picks to
    expose is indeterminate. This can cause GHC to pick the wrong installed
    package and to report that it cannot load a module because it is a member of
    a hidden package.

## Compilation

The source file can be compiled by passing either the `--compile` flag (no
optimization) or the `--optimize` flag (compilation with optimization). If the
file is compiled, passing the `--no-run` flag will mean the compiled code is not
run.

By default, all the compilation outputs (including the executable) are written
to the directory of the source file. Pass the `--use-root` flag to write such
outputs to a script-specific location in the `scripts` directory of the Stack
root. The location reflects the absolute path to the source file, but ignoring
the drive. This can avoid clutter in the source file directory.

## GHC options

Additional options can be passed to GHC using the `--ghc-options` option.

## Script arguments

Everything after `--` on the command line is interpreted as a command line
argument to be passed to what is run.

## Examples

### Example 1

A Haskell source file `MyScript.hs` at location
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
stack --snapshot lts-24.9 script --package acme-missiles --compile MyScript.hs -- "Don't panic!" "Duck and cover!"
~~~

`acme-missiles-0.3` (the most recent version in the package index) will be used.

All the compilation outputs (like `Main.hi`, `Main.o`, and the executable
`MyScript`) will be written to the `my-project` directory.

If compiled and run with the additional flag `--use-root`, all the compilation
outputs will be written to a directory named `MyScript.hs` at
`Users/jane/my-project/` in the `scripts` directory of the Stack root.

### Example 2

As for Example 1, but `acme-missiles-0.2` is specified by adding it to the
snapshot as an extra-dep. The `stack script` command is specified using Stack's
[script interpreter](../topics/scripts.md).

~~~haskell
{- stack script
   -- snapshot lts-24.9
   -- extra-dep acme-missiles-0.2
   -- package acme-missiles
-}
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

~~~text
stack MyScript.hs "Don't panic!" "Duck and cover!"
~~~

### Example 3

Stackage snapshot LTS Haskell 20.25 includes GHC boot package `Win32` directly.
On Windows only, GHC boot packages `Cabal`, `directory`, `process` and `time`
all depend on `Win32` and, consequently, are all treated as 'replaced'.
Consequently, for example, Stack will:

* on Windows, try to construct a build plan based on the latest version of
  `Cabal` in the package index; and
* on non-Windows, use the boot package in the build plan (because `Cabal` is not
  'replaced').

Consider also the following script extract, based on snapshot Stackage
 LTS Haskell 20.25, where considerations on Windows differ from non-Windows. The
`stack script` command is specified using Stack's
[script interpreter](../topics/scripts.md).

=== "Windows"

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

### Example 4

A Haskell source file `MyScript.hs`, as follows:

~~~haskell
{- stack script
   --snapshot lts-24.9
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Main (main) where

import "text" Data.Text (Text (..), unpack)

main :: IO ()
main = putStrLn $ unpack "This is text."
~~~

As module `Data.Text` is exposed by a number of packages that are included,
directly or indirectly, in the specified snapshot (`incipit-base`,
`incipit-core`, `relude` and `text`), `PackageImports` and `"text"` are required
to specify which module is being imported.

### Example 5

Stackage snapshot LTS Haskell 23.18 specifies Cabal package `vector-0.13.2.0`
which includes public sub-library `benchmarks-O2`. The sub-library exposes
module `Bench.Vector.TestData.ParenTree` which exports `parenTree`. The
following is a valid script:

~~~haskell
{- stack script
   --snapshot lts-23.18
   --package z-vector-z-benchmarks-O2
-}
{-# LANGUAGE LambdaCase #-}

import Bench.Vector.TestData.ParenTree ( parenTree )
import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "An initial argument is required."
  (arg:_) -> do
    let n = read arg
    if n >= 0 && even n
      then do
        putStrLn "A balanced binary tree structure"
        putStrLn $ "with " <> show n <> " parentheses positions:"
        print $ parenTree n
      else
        putStrLn "A positive even integer argument is required."
~~~
