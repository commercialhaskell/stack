# `stack` Quick Start Guide

If not already done you first need to
[install `stack`](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md).

## Start your new project:

~~~ {.bash}
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe
~~~

- The `stack new` command will create a new directory containing all
the needed files to start a project correctly.
- The `stack setup` will download the compiler if necessary.
- The `stack build` command will build the minimal project.
- `stack exec my-project-exe` will execute the command.

If you want to launch a REPL:

~~~ {.bash}
stack ghci
~~~

## Workflow

The `stack new` command should have created the following files:

~~~
.
├── LICENSE
├── Setup.hs
├── app
│   └── Main.hs
├── my-project.cabal
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

    3 directories, 7 files
~~~

So to manage your library:

1. Edit files in the `src/` directory.

The `app` directory should preferably contains only files related to
executables.

2. If you need to include another library (for example the package [`text`](https://hackage.haskell.org/package/text):

   - Add the package `text` to the file `my-project.cabal`
     in the section `build-depends: ...`.
   - run `stack build` another time

3. If you get an error that tells you your package isn't in the LTS.
   Just try to add a new version in the `stack.yaml` file in the `extra-deps` section.

It was a really fast introduction on how to start to code in Haskell using `stack`.
If you want to go further, we highly recommend you to read the [`stack` guide](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md).
