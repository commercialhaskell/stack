# Architecture

## Terminology

* Package identifier: a package name and version, e.g. text-1.2.1.0
* GhcPkgId: a package identifier plus the unique hash for the generated binary,
  e.g. text-1.2.1.0-bb83023b42179dd898ebe815ada112c2
* Package index: a collection of packages available for download. This is a
  combination of an index containing all of the .cabal files (either a tarball
  downloaded via HTTP(S) or a Git repository) and some way to download package
  tarballs.
    * By default, stack uses a single package index (the Github/S3 mirrors of
      Hackage), but supports customization and adding more than one index
* Package database: a collection of metadata about built libraries
* Install root: a destination for installing packages into. Contains a bin path
  (for generated executables), lib (for the compiled libraries), pkgdb (for the
  package database), and a few other things
* Snapshot: an LTS Haskell or Stackage Nightly, which gives information on a
  complete set of packages. This contains a lot of metadata, but importantly it
  can be converted into a mini build plan...
* Mini build plan: a collection of package identifiers and their build flags
  that are known to build together
* Resolver: the means by which stack resolves dependencies for your packages.
  The two currently supported options are snapshot (using LTS or Nightly), and
  GHC (which installs no extra dependencies). Others may be added in the future
  (such as a SAT-based dependency solver). These packages are always taken from
  a package index
* extra-deps: additional packages to be taken from the package index for
  dependencies. This list will *shadow* packages provided by the resolver
* Local packages: source code actually present on your file system, and
  referred to by the `packages` field in your stack.yaml file. Each local
  package has exactly one .cabal file
* Project: a stack.yaml config file and all of the local packages it refers to.

## Databases

Every build uses three distinct install roots, which means three separate
package databases and bin paths. These are:

* Global: the packages that ship with GHC. We never install anything into this
  database
* Snapshot: a database shared by all projects using the same snapshot. Packages
  installed in this database must use the exact same dependencies and build
  flags as specified in the snapshot, and cannot be affected by user flags,
  ensuring that one project cannot corrupt another. There are two caveats to
  this:
    * If different projects use different package indices, then their
      definitions of what package foo-1.2.3 are may be different, in which case
      they *can* corrupt each other's shared databases. This is warned about in
      the FAQ
    * Turning on profiling may cause a package to be recompiled, which will
      result in a different GhcPkgId
* Local: extra-deps, local packages, and snapshot packages which depend on them
  (more on that in shadowing)

## Building

### Shadowing

Every project must have precisely one version of a package. If one of your
local packages or extra dependencies conflicts with a package in the snapshot,
the local/extradep *shadows* the snapshot version. The way this works is:

* The package is removed from the list of packages in the snapshot
* Any package that depends on that package (directly or indirectly) is moved
  from the snapshot to extra-deps, so that it is available to your packages as
  dependencies.
    * Note that there is no longer any guarantee that this package will build,
      since you're using an untested dependency

After shadowing, you end up with what is called internally a `SourceMap`, which
is `Map PackageName PackageSource`, where a `PackageSource` can be either a
local package, or a package taken from a package index (specified as a version
number and the build flags).

### Installed packages

Once you have a `SourceMap`, you can inspect your three available databases and
decide which of the installed packages you wish to use from them. We move from
the global, to snapshot, and finally local, with the following rules:

* If we require profiling, and the library does not provide profiling, do not
  use it
* If the package is in the `SourceMap`, but belongs to a difference database,
  or has a different version, do not use it
* If after the above two steps, any of the dependencies are unavailable, do not
  use it
* Otherwise: include the package in the list of installed packages

We do something similar for executables, but maintain our own database of
installed executables, since GHC does not track them for us.

### Plan construction

When running a build, we know which packages we want installed (inventively
called "wanteds"), which packages are available to install, and which are
already installed. In plan construction, we put them information together to
decide which packages must be built. The code in Stack.Build.ConstructPlan is
authoritative on this and should be consulted. The basic idea though is:

* If any of the dependencies have changed, reconfigure and rebuild
* If a local package has any files changed, rebuild (but don't bother
  reconfiguring)
* If a local package is wanted and we're running tests or benchmarks, run the
  test or benchmark even if the code and dependencies haven't changed

### Plan execution

Once we have the plan, execution is a relatively simple process of calling
`runghc Setup.hs` in the correct order with the correct parameters. See
Stack.Build.Execute for more information.

## Configuration

stack has two layers of configuration: project and non-project. All of these
are stored in stack.yaml files, but the former has extra fields (resolver,
packages, extra-deps, and flags). The latter can be monoidally combined so that
a system config file provides defaults, which a user can override with
`~/.stack/config.yaml`, and a project can further customize. In addition,
environment variables STACK\_ROOT and STACK\_YAML can be used to tweak where
stack gets its configuration from.

stack follows a simple algorithm for finding your project configuration file:
start in the current directory, and keep going to the parent until it finds a
`stack.yaml`. When using `stack ghc` or `stack exec` as mentioned above, you'll
sometimes want to override that behavior and point to a specific project in
order to use its databases and bin directories. To do so, simply set the
`STACK_YAML` environment variable to point to the relevant `stack.yaml` file.

## Snapshot auto-detection

When you run `stack build` with no stack.yaml, it will create a basic
configuration with a single package (the current directory) and an
auto-detected snapshot. The algorithm it uses for selecting this snapshot is:

* Try the latest two LTS major versions at their most recent minor version
  release, and the most recent Stackage Nightly. For example, at the time of
  writing, this would be lts-2.10, lts-1.15, and nightly-2015-05-26
* For each of these, test the version bounds in the package's .cabal file to
  see if they are compatible with the snapshot, choosing the first one that
  matches
* If no snapshot matches, uses the most recent LTS snapshot, even though it
  will not compile

If you end up in the no compatible snapshot case, you typically have three
options to fix things:

* Manually specify a different snapshot that you know to be compatible. If you
  can do that, great, but typically if the auto-detection fails, it means that
  there's no compatible snapshot
* Modify version bounds in your .cabal file to be compatible with the selected
  snapshot
* Add `extra-deps` to your stack.yaml file to fix compatibility problems

Remember that running `stack build` will give you information on why your build
cannot occur, which should help guide you through the steps necessary for the
second and third option above. Also, note that those options can be
mixed-and-matched, e.g. you may decide to relax some version bounds in your
.cabal file, while also adding some extra-deps.

## Explicit breakage

As mentioned above, updating your package indices will not cause stack to
invalidate any existing package databases. That's because stack is always
explicit about build plans, via:

1. the selected snapshot
2. the extra-deps
3. local packages

The only way to change a plan for packages to be installed is by modifying one
of the above. This means that breakage of a set of installed packages is an
*explicit* and *contained* activity. Specifically, you get the following
guarantees:

* Since snapshots are immutable, the snapshot package database will not be
  invalidated by any action. If you change the snapshot you're using, however,
  you may need to build those packages from scratch.
* If you modify your extra-deps, stack may need to unregister and reinstall
  them.
* Any changes to your local packages trigger a rebuild of that package and its
  dependencies.
