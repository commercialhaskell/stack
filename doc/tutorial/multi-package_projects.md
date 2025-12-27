  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 9. Multi-package projects

Everything we have done with Stack so far has used a single-package project,
where the project directory is also the package's directory. However, a Stack
project can have more than one project package.

Let us demonstrate this with a project that has two project packages named
`packageA` and `packageB`. We will create a project directory named `my-project`
and, for our example, create the two project packages in subdirectories.
Command:

~~~text
mkdir my-project
cd my-project
stack new packageA --no-init
stack new packageB --no-init
stack init
~~~

The `--no-init` flags above stop Stack from creating project-level configuration
files in the `packageA` and `packageB` directories that
[`stack new`](../commands/new_command.md) will create.

The [`stack init`](../commands/init_command.md) command above creates a
project-level configuration file (`stack.yaml`) in the `my-project` directory.
The command should report something like this:

~~~text
Looking for Cabal or package.yaml files to use to initialise Stack's
project-level YAML configuration file.

Using the Cabal packages:
* packageA\
* packageB\

Selecting the best among 14 snapshots...

Note: Matches https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/24/25.yaml

Selected the snapshot https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/24/25.yaml.
Initialising Stack's project-level configuration file using snapshot https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/24/25.yaml.
Considered 2 user packages.
Writing configuration to stack.yaml.
Stack's project-level configuration file has been initialised.
~~~

Ignoring comments in the file, the content of the created `stack.yaml` file
should be something like this:

~~~yaml
snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/24/25.yaml

packages:
- packageA
- packageB
~~~

The value of the [`packages`](../configure/yaml/project.md#packages) key is a
list of paths (relative paths, in this example) to project package directories.

If we command
[`stack ide targets`](../commands/ide_command.md#the-stack-ide-targets-command),
Stack reports the build targets for these two project packages:

~~~text
packageA:lib
packageA:exe:packageA-exe
packageA:test:packageA-test
packageB:lib
packageB:exe:packageB-exe
packageB:test:packageB-test
~~~

If we command
[`stack build`](../commands/build_command.md#no-targets-specified), Stack will
build all the library and executable components of all the project packages.

One project package can depend on another. Let us demonstrate this by modifying
the main library of the `packageB` package to depend on that of the `packageA`
package.

Currently, the source code of the `packageA` and `packageB` packages are the
same. Let us first modify the `someFunc` function exported by the `Lib` module
exposed by the `packageA` package, as follows:

~~~haskell
someFunc :: IO ()
someFunc = putStrLn "someFunc of packageA's Lib module"
~~~

and the source code of the `Lib` module exposed by the `packageB` package to
become:

~~~haskell
{-# LANGUAGE PackageImports #-}

module Lib
    ( someFunc
    ) where

import qualified "packageA" Lib as LibA

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc of packageB's Lib module"
    LibA.someFunc
~~~

In this example, as the `packageA` and `packageB` packages both expose a module
named `Lib`, we have to use GHC's language extension
[`PackageImports`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html)
to allow imports from the `Lib` module exposed by the `packageA` package to be
distiguished.

In the package description file (`package.yaml`) for the `packageB` package, we
need to specify that the dependencies of its main library now include the main
library of the `packageA` package, as follows (extract):

~~~yaml
library:
  source-dirs: src
  dependencies:
  - packageA # Add the dependency on the main library of the packageA package
~~~

Now, if we command `stack build packageB`, Stack will build the library and
executable components of the `packageA` package (the dependency) and then the
library and executable (named `packageB-exe`) of `packageB`.

To execute the built `packageB-exe` executable, we can command:

~~~text
stack exec packageB-exe
~~~

giving the expected output:

~~~text
someFunc of packageB's Lib module
someFunc of packageA's Lib module
~~~

!!! note

    A project package can depend on another project package, as above. It can
    also depend on a local package that is specified as an
    [extra-dep](../configure/yaml/project.md#extra-deps). Although both
    dependencies are local, the former is part of the project and the latter is
    not.
