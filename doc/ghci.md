# GHCi

`stack ghci` allows you to load components and files of your project into
`ghci`. It uses the same TARGET syntax as `stack build`, and can also take
options like `--test`, `--bench`, and `--flag`.  Similarly to `stack build`, the
default is to load up ghci with all libraries and executables in the project.

In order to load multiple components, `stack ghci` combines all of the ghc options
together.  This doesn't work in the general case, but when the packages being
loaded share similar conventions, it should work out.  A common source of issues
is when one component defines default extensions which aren't assumed by another
component.  For example, specifying `NoImplicitPrelude` in one component but
not another is quite likely to cause failures.  `ghci` will be run with
`-XNoImplicitPrelude`, but it is likely that modules in the other component
assume that the Prelude is implicitly imported.

## Selecting Main module

When loading multiple packages, there may be multiple definitions for the `Main`
module.  You can specify which Main module to load by passing in the
`--main-is TARGET` flag.  If no selection is made and there are multiple `Main`
modules, you will be asked to select from a list of options.

## Speeding up initial load

There are two ways to speed up the initial startup of ghci:

* `--no-build`, to skip an initial build step.  This only works if the
  dependencies have already been built.

* `--no-load`, to skip loading all defined modules into ghci.  You can then
  directly use `:load MyModule` to load a specific module in your project.

## Loading a filepath directly

Instead of the `TARGET` syntax, it is also possible to directly run
`stack ghci src/MyFile.hs`.  This will figure out which component the file is
associated with, and use the options from that component.

## Specifying extra packages to build / depend on

Sometimes you want to load ghci with an additional package, that isn't a direct
dependency of your components.  This can be achieved by using the `--package` flag.
For example, if I want to experiment with the lens library, I can run
`stack ghci --package lens`.

## Running plain ghci

`stack ghci` always runs ghci configured to load code from packages in your
project.  In particular, this means it passes in flags like `-hide-all-packages`
and `-package-id=` in order to configure which packages are visible to ghci.

For doing experiments which just involve packages installed in your databases,
it may be useful to run ghci plainly like `stack exec ghci`. This will run a
plain `ghci` in an environment which includes `GHC_PACKAGE_PATH`, and so will
have access to your databases.
