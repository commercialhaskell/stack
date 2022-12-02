<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack ghci` and `stack repl` commands

~~~text
stack ghci [TARGET/FILE] [--pedantic] [--ghci-options OPTIONS]
           [--ghc-options OPTIONS] [--flag PACKAGE:[-]FLAG] [--with-ghc GHC]
           [--[no-]load] [--package PACKAGE] [--main-is TARGET]
           [--load-local-deps] [--[no-]package-hiding] [--only-main] [--trace]
           [--profile] [--no-strip] [--[no-]test] [--[no-]bench]
~~~

A read–evaluate–print loop (REPL) environment takes single user inputs, executes
them, and returns the result to the user. GHCi is GHC's interactive environment.
The `stack ghci` or `stack repl` commands, which are equivalent, allow you to
load components and files of your project into GHCi.

The command uses the same TARGET syntax as `stack build`. It can also take flags
like `--test` and `--bench` and options like `--flag`. Similarly to
`stack build`, the default is to load up GHCi with all libraries and executables
in the project.

In order to load multiple components, `stack ghci` combines all of the GHC
options together. This doesn't work in the general case, but when the packages
being loaded share similar conventions, it should work out. A common source of
issues is when one component defines default extensions which aren't assumed by
another component. For example, specifying `NoImplicitPrelude` in one component
but not another is quite likely to cause failures. GHCi will be run with
`-XNoImplicitPrelude`, but it is likely that modules in the other component
assume that the `Prelude` is implicitly imported.

## Selecting Main module

When loading multiple packages, there may be multiple definitions for the `Main`
module. You can specify which `Main` module to load with the
`--main-is <target>` option. If no selection is made and there are multiple
`Main` modules, you will be asked to select from a list of options.

## Speeding up initial load

There are two ways to speed up the initial startup of GHCi:

1.  Pass the `--no-build` flag, to skip an initial build step. This works only
    if the dependencies have already been built.

2.  Pass the `--no-load` flag, to skip loading all defined modules into GHCi.
    You can then directly use `:load MyModule` in GHCi to load a specific module
    in your project.

## Loading just the main module

By default, `stack ghci` loads and imports all of the modules in the package.
This allows you to easily use anything exported by your package. This is usually
quite convenient, but in some cases it makes sense to only load one module, or
no modules at all. The `--only-main` flag allows this. It specifies that only
the main module will be loaded, if any. This is particularly useful in the
following circumstances:

1. You're loading the project in order to run it in GHCi (e.g. via `main`), and
   you intend to reload while developing. Without the `--only-main` flag, you
   will need to quit and restart GHCi whenever a module gets deleted. With the
   flag, reloading should work fine in this case.

2. If many of your modules have exports named the same thing, then you'll need
   to refer to them using qualified names. To avoid this, it may be easier to
   use the `--only-main` flag to start with a blank slate and just import the
   modules you are interested in.

## Loading a filepath directly

Instead of the `TARGET` syntax, it is also possible to command directly, for
example:

~~~text
stack ghci src/MyFile.hs
~~~

This will figure out which component the file is associated with, and use the
options from that component.

## Specifying extra packages to build or depend on

Sometimes you want to load GHCi with an additional package, that isn't a direct
dependency of your components. This can be achieved by using the `--package`
option. For example, if I want to experiment with the `lens` library, I can
command:

~~~text
stack ghci --package lens
~~~

## Running plain GHCi

`stack ghci` always runs GHCi configured to load code from packages in your
project. In particular, this means it passes in flags like `-hide-all-packages`
and `-package-id=` in order to configure which packages are visible to GHCi.

For doing experiments which just involve packages installed in your databases,
it may be useful to run GHCi plainly like:

~~~text
stack exec ghci
~~~

This will run a plain GHCi in an environment which includes `GHC_PACKAGE_PATH`,
and so will have access to your databases.

!!! note

    Running `stack ghci` on a pristine copy of the code doesn't currently build
    libraries
    (issue [#2790](https://github.com/commercialhaskell/stack/issues/2790)) or
    internal libraries
    (issue [#4148](https://github.com/commercialhaskell/stack/issues/4148)). It
    is recommended to always use `stack build` before using `stack ghci`, until
    these two issues are closed.
