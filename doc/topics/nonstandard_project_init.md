<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Non-standard project initialization

You may need to configure Stack to work with an existing project that has one or
more Cabal files but no Stack project-level configuration file (`stack.yaml`, by
default).

## The `stack init` command

The `stack init` command:

* finds all of the Cabal files in your current directory and subdirectories
  (unless you use `--ignore-subdirs`) and determines the packages and versions
  they require
* Finds the best combination of snapshot and package flags that allows
  everything to compile with minimum external dependencies
* Tries to look for the best matching snapshot from latest Haskell LTS, latest
  Stackage Nightly, and other Haskell LTS, in that order

If `stack init` finds a match, it will generate a `stack.yaml` file.

You can specify the directory, or directories to include in the search for
Cabal files.

### The `stack init --force` flag

Set the flag to force the over-writing of any existing `stack.yaml` file.

### The `stack init --ignore-subdirs` flag

Set the flag to not search for Cabal files in subdirectories.

### The `stack init --omit-packages` flag

Set the flag to exclude any conflicting or incompatible user packages.
