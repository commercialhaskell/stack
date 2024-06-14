<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Build overview

!!! warning

    This document should not be considered accurate until this warning is
    removed.

    This is a work-in-progress document covering the build process used by
    Stack. It was started following the Pantry rewrite work in Stack 2.1.1, and
    contains some significant changes/simplifications from how things used to
    work. This document will likely not fully be reflected in the behavior of
    Stack itself until late in the Stack 2.0 development cycle.

## Terminology

* Project package: anything listed in `packages` in stack.yaml
* Dependency: anything listed in extra-deps or a snapshot
* Target: package and/or component listed on the command line to be built. Can
  be either project package or dependency. If none specified, automatically
  targets all project packages
* Immutable package: a package which comes from Hackage, an archive, or a
  repository. In contrast to...
* Mutable package: a package which comes from a local file path. The contents
  of such a package are assumed to mutate over time.
* Write only database: a package database and set of executables for a given set
  of _immutable_ packages. Only packages from immutable sources and which
  depend exclusively on other immutable packages can be in this database.
  *NOTE* formerly this was the _snapshot database_.
* Mutable database: a package database and set of executables for packages which
  are either mutable or depend on such mutable packages. Importantly, packages
  in this database can be unregister, replaced, etc, depending on what happens
  with the source packages. *NOTE* formerly this was the *local database*.

Outdated terminology to be purged:

* Wanted
* Local
* Snapshot package

## Inputs

Stack pays attention to the following inputs:

* Current working directory, used for finding the default `stack.yaml` file and
  resolving relative paths
* The `STACK_YAML` environment variable
* Command line arguments (CLI args), as will be referenced below

Given these inputs, Stack attempts the following process when performing a build.

## Find the `stack.yaml` file

* Check for a `--stack-yaml` CLI arg, and use that
* Check for a `STACK_YAML` env var
* Look for a `stack.yaml` in this directory or ancestor directories
* Fall back to the default global project

This file is parsed to provide the following config values:

* `snapshot` (or, alternatively, `resolver`) (required field)
* `compiler` (optional field)
* `packages` (optional field, defaults to `["."]`)
* `extra-deps` (optional field, defaults to `[]`)
* `flags` (optional field, defaults to `{}`)
* `ghc-options` (optional field, defaults to `{}`)

`flags` and `ghc-options` break down into both _by name_ (applied to a
specific package) and _general_ (general option `*` for flags is only available
in CLI).

## Wanted compiler, dependencies, and project packages

* If the `--snapshot` CLI is present, ignore the `snapshot` (or `resolver`) and
  `compiler` config values
* Load up the indicated snapshot (either config value or CLI arg). This will
  provide:
    * A map from package name to package location, flags, GHC options,
      and if a package should be hidden. All package locations here
      are immutable.
    * A wanted compiler version, e.g. `ghc-8.6.5`
* If the `--compiler` CLI arg is set, or the `compiler` config value
  is set (and `--snapshot` CLI arg is not set), ignore the wanted
  compiler from the snapshot and use the specified wanted compiler
* Parse `extra-deps` into a `Map PackageName PackageLocation`,
  containing both mutable and immutable package locations. Parse
  `packages` into a `Map PackageName ProjectPackage`.
* Ensure there are no duplicates between these two sets of packages
* Delete any packages from the snapshot packages that appear in
  `packages` or `extra-deps`
* Perform a left biased union between the immutable `extra-deps`
  values and the snapshot packages. Ignore any settings in the
  snapshot packages that have been replaced.
* Apply the `flags` and `ghc-options` by name to these packages overwriting
  any previous values coming from a snapshot. If any values are specified
  but no matching package is found, it's an error. If a flag is not defined
  in the corresponding package cabal file, it's an error.
* We are now left with the following:
    * A wanted compiler version
    * A map from package name to immutable packages with package config (flags,
      GHC options, hidden)
    * A map from package name to mutable packages as dependencies with package
      config
    * A map from package name to mutable packages as project packages with
      package config

## Get actual compiler

Use the wanted compiler and various other Stack config values (not all
listed here) to find the actual compiler, potentially installing it in
the process.

## Global package sources

With the actual compiler discovered, list out the packages available
in its database and create a map from package name to
version/GhcPkgId. Remove any packages from this map which are present
in one of the other three maps mentioned above.

## Resolve targets

Take the CLI args for targets as raw text values and turn them into
actual targets.

* Do a basic parse of the values into one of the following:
    * Package name
    * Package identifier
    * Package name + component
    * Directory
* An empty target list is equivalent to listing the package names of
  all project packages
* For any directories specified, find all project packages in that
  directory or subdirectories therefore and convert to those package
  names
* For all package identifiers, ensure that either the package name
  does not exist in any of the three parsed maps from the "wanted
  compiler" step above, or that the package is present as an immutable
  dependency from Hackage. If so, create an immutable dependency entry
  with default flags, GHC options, and hidden status, and add this
  package to the set of immutable package dependencies.
* For all package names, ensure the package is in one of the four maps
  we have, and if so add to either the dependency or project package
  target set.
* For all package name + component, ensure that the package is a
  project package, and add that package + component to the set of
  project targets.
* Ensure that no target has been specified multiple times. (*FIXME*
  Mihai states: I think we will need an extra consistency step for
  internal libraries. Sometimes stack needs to use the mangled name
  (`z-package-internallibname-z..`), sometimes the
  `package:internallibname` one. But I think this will become obvious
  when doing the code changes.)

We now have an update four package maps, a new set of dependency
targets, and a new set of project package targets (potentially with
specific components).

## Apply named CLI flags

Named CLI flags are applied to specific packages by updating the
config in one of the four maps. If a flag is specified and no package
is found, it's an error. Note that flag settings are added _on top of_
previous settings in this case, and does not replace them. That is, if
previously we have `singleton (FlagName "foo") True` and now add
`singleton (FlagName "bar") True`, both `foo` and `bar` will now be
true. If any flags are specified but no matching package is found,
it's an error. If a flag is not defined in the corresponding package
cabal file, it's an error.

## Apply CLI GHC options

CLI GHC options are applied as general GHC options according to
`apply-ghc-options` setting.

## Apply general flags from CLI

`--flag *:flagname[:bool]` specified on the CLI are applied to any
project package which uses that flag name.

## Apply general GHC options

General options are divided into the following categories:

* `$locals` is deprecated, it's now a synonym for `$project`
* `$project` applies to all project packages, not to any dependencies
* `$targets` applies to all project packages that are targets, not to any
  dependencies or non-target project packages. This is the default option
  for `apply-ghc-options`
* `$everything` applies to all packages in the source map excluding
  global packages

These options get applied to any corresponding packages in
the source map. If some GHC options already exist for such a package then
they get prepended otherwise they get used as is.

## Determine snapshot hash

Use some deterministic binary serialization and SHA256 thereof to get
a hash of the following information:

* Actual compiler (GHC version, path, *FIXME* probably some other
  unique info from GHC, I've heard that `ghc --info` gives you
  something)
* Global database map
* Immutable dependency map

Motivation: Any package built from the immutable dependency map and
installed in this database will never need to be rebuilt.

!!! bug "To do"

    Caveat: do we need to take profiling settings into account here? How about
    Haddock status?

## Determine actual target components

* Dependencies: "default" components (all libraries and executables)
* Project packages:
    * If specific components named: only those, plus any libraries present
    * If no specific components, include the following:
        * All libraries, always
        * All executables, always
        * All test suites, _if_ `--test` specified on command line
        * All benchmarks, _if_ `--bench` specified on command line

## Construct build plan

* Applied to every target (project package or dependency)
* Apply flags, platform, and actual GHC version to resolve
  dependencies in any package analyzed
* Include all library dependencies for all enabled components
* Include all dependencies for tools used during building ('build tools') for
  all enabled components (using the fun backwards compat logic for
  `build-tools`)
* Apply the logic recursively to come up with a full build plan
* If a task depends exclusively on immutable packages, mark it as
  immutable. Otherwise, it's mutable. The former go into the snapshot
  database, the latter into the local database.

We now have a set of tasks of packages/components to build, with full
config information for each package, and dependencies that must be
built first.

!!! bug "To do"

    There's some logic to deal with cyclic dependencies between test suites and
    benchmarks, where a task can be broken up into individual components versus
    be kept as a single task. Need to document this better. Currently it's the
    "all in one" logic.

## Unregister local modified packages

* For all mutable packages in the set of tasks, see if any files have
  changed since last successful build and, if so, unregister + delete
  their executables
* For anything which depends on them directly or transitively,
  unregister + delete their executables

## Perform the tasks

* Topological sort, find things which have no dependencies remaining
* Check if already installed in the relevant database
    * Check package database
    * Check Stack specific "is installed" flags, necessary for
      non-library packages
    * For project packages, need to also check which components were
      built, if tests were run, if we need to rerun tests, etc
* If all good: do nothing
* Otherwise, for immutable tasks: check the precompiled cache for an
  identical package installation (same GHC, dependencies, etc). If
  present: copy that over, and we're done.
* Otherwise, perform the build, register, write to the Stack specific
  "is installed" stuff, and (for immutable tasks) register to the
  precompiled cache

"Perform the build" consists of:

* Do a cabal configure, if needed
* Build the desired components
* For all test suites built, unless "no rerun tests" logic is on and
  we already ran the test, _or_ "no run tests" is on, run the test
* For all benchmarks built, unless "no run benchmarks" is on, run the
  benchmark
