__NOTE__ Needs to be cleaned up, this content hasn't been updated since some major refactorings took place.

## Architecture of stack

It seemlessly handles multi-package projects,  It is:

* designed from the ground up to support common workflows many developers
  encounter in the real world, such as multi-package projects and shared
  package databases

* robust by default, ensuring that separate projects cannot corrupt other
  package databases (no explicit sandboxing required)

* user friendly, making standard operations simple and performing necessary
  prerequisites automatically, like installing dependencies and reconfiguring

* modular, taking advantage of existing high quality projects like [LTS
  Haskell](https://github.com/fpco/lts-haskell),
  [Stackage](https://www.stackage.org/),
  [all-cabal-hashes](https://github.com/commercialhaskell/all-cabal-hashes),
  [the S3 Haskell package
  mirror](https://www.fpcomplete.com/blog/2015/03/hackage-mirror),
  [shake](http://shakebuild.com/) and the
  [Cabal library](http://www.stackage.org/package/Cabal)

* explicit about changes to your install plan; updating the list of available
  packages will never cause `stack` to suddenly corrupt and reinstall your
  packages

stack chooses sensible defaults wherever possible (such as preferring LTS
Haskell snapshots) while allowing simple user customization (such as switching
to either Stackage Nightly or manual package dependency selection). It will
download and install GHC for you on many common platforms, or use your
preexisting installation.

#### Typical user story

To give an idea of how stack is used, here's a typical user story for a simple
use case. Don't worry if some of the details seem strange, they'll be explained
throughout this document:

1. You download the `stack` executable from (**FIXME** explain where it gets downloaded from) and put it on your `PATH`
2. Go to an existing Haskell project with a .cabal file (we'll explain multi-package projects below)
3. Run `stack build`. This will notice that you don't have a `stack.yaml` project configuration file, and create one for you. It will identify the most compatible snapshot (either LTS Haskell or Stackage Nightly), and set the current directory as the only package in the project. (See section below on snapshot auto-selection for more details.)
4. Once the stack.yaml file is written, stack will check if you have the correct version of GHC on your PATH. If so, it will use it and continue. Otherwise, it will recommend that your run `stack setup` to download and install that GHC version. You can then run `stack build` again to continue.
5. stack analyzes your package's cabal file, finds all necessary dependencies, and then installs the appropriate versions based on your selected snapshot. These packages are installed to a snapshot-specific package database, meaning that other projects using the same snapshot can use them, but other projects using a different snapshot will be unaffected by them
6. Once your dependencies are installed, your package will be built and installed to your project-specific database
7. If you have build errors, stack will print them
8. Run `stack test` to run your test suites
9. If you make changes to your package (or any packages in a multi-package project), `stack build` will rebuild them

### stack.yaml

stack.yaml is a project-wide configuration file. In stack terminology, a
project is a collection of 1 or more packages, where a package is a single
.cabal file. As its name implies, this is a YAML configuration file. You can
look at the stack.yaml automatically created for you and add on from there.  A
real life example from the [WAI repository](https://github.com/yesodweb/wai)
is:

```yaml
resolver: lts-2.9
packages:
- ./wai
- ./wai-extra
- ./warp
- ./warp-tls
- ./wai-app-static
- ./wai-handler-fastcgi
- ./wai-handler-launch
- ./wai-websockets
- ./wai-conduit
- ./mime-types
- ./auto-update
extra-deps:
- fast-logger-2.3.1
- wai-logger-2.2.4
```

This demonstrates the three most common configuration values:

* `resolver` states how dependencies are determined. Typically this will be an LTS Haskell or Stackage Nightly version, e.g. `lts-2.9` or `nightly-2015-05-26`. If you would like to specify all your dependencies manually, you can simply state the GHC major version to be used via `ghc-7.8` or `ghc-7.10`
* `packages` lists the locations of all packages to be built. You generally will want to make these relative locations. A common use case- like that of WAI- is to put a `stack.yaml` at the root of a Git repository, with subdirectories for each package. If you just have a single package, it's also quite common to use something like `packages: ["."]`
* `extra-deps` specifies additional dependencies to be installed from upstream. This can be used for multiple purposes: overriding a package version selected by your snapshot, or dealing with dependencies on your local packages (described below)

__FIXME describe all of the other settings__

### Dependencies vs local packages

There's a clear separation in stack between your *dependencies* and your *local
packages*. When installing dependencies for a snapshot, your local packages are
not taken into account at all, and all installation goes into the shared
package database. This is what allows us to both reuse compiled libraries and
not risk corruption of a database based on what other projects are doing.

When building this shared snapshot database, we turn on profiling immediately.
The result of this is that the initial build will be a bit slower, but since
recompilation of those packages never needs to happen, it's overall a
time-saving feature.

For local packages, however, stack will intelligently note when requirements
have changed and rebuild. For example, when you initially run `stack build`, it
will perform a non-profiled build. Later turning on profiling will
automatically trigger local packages to be recompiled. The same applies to
changing flags like `-Wall` and `-Werror`.

#### Corner case: dependencies on local packages

One corner case to be noted is when you are replacing a package in the snapshot
with a local package. In the example from WAI above, note that the lts-2.9
snapshot being used already provides the wai package (and others). In this
case, stack will prune the dependency tree to disallow usage of wai and any of
its users from the LTS snapshot. This ensures that when building your code,
there is only one copy of the wai library in play- the local one. In the
stack.yaml example above, we needed to explicitly include fast-logger and
wai-logger since they depend on some of the packages being built locally.

If this "corner case" seems a bit confusing, don't worry about it too much,
most users will not run into it. Over time, as others get more experience with
this case, I hope we can expand this section to make more sense.

### Shared databases, not sandboxes

Since sandboxing is such a common feature in the rest of the Haskell build tool
world (hsenv, cabal, cabal-dev), it may be surprising to see so little mention
of it in this document. That's because the goal of sandboxing- isolating
builds- is the default in stack. There are two different aspects of a build
that warrant some kind of isolation:

* The package database, containing information on the compiled libraries
* The bin directory, containing generated executables

Whenever you build with stack, there are three of each at play:

* GHC's `bin` directory and global package database
* The snapshot's (e.g., lts-2.9)
* Your project

These are layered so that they shadow the previous ones. If you use extra-deps
to install a new version of `text` in your project, for example, that will
override whatever is installed in your snapshot. If you install a newer version
of the xhtml library via your snapshot, it will shadow the xhtml that ships
with GHC.

The one important difference to note versus how GHC typically behaves is that
we don't make use of the "user package database" at all. This is to properly
enforce isolation. If you wish to use tools like `ghc` or `runhaskell`, you
should do so via `stack ghc` or `stack exec`.

### Environment variables

stack follows a simple algorithm for finding your project configuration file:
start in the current directory, and keep going to the parent until it finds a
`stack.yaml`. When using `stack ghc` or `stack exec` as mentioned above, you'll
sometimes want to override that behavior and point to a specific project in
order to use its databases and bin directories. To do so, simply set the
`STACK_YAML` environment variable to point to the relevant `stack.yaml` file.

### Snapshot auto-detection

When you run `stack build` with no stack.yaml, it will create a basic
configuration with a single package (the current directory) and an
auto-detected snapshot. The algorithm it uses for selecting this snapshot is:

* Try the lastest two LTS major versions at their most recent minor version release, and the most recent Stackage Nightly. For example, at the time of writing, this would be lts-2.10, lts-1.15, and nightly-2015-05-26
* For each of these, test the version bounds in the package's .cabal file to see if they are compatible with the snapshot, choosing the first one that matches
* If no snapshot matches, uses the most recent LTS snapshot, even though it will not compile

If you end up in the no compatible snapshot case, you typically have three options to fix things:

* Manually specify a different snapshot that you know to be compatible. If you can do that, great, but typically if the auto-detection fails, it means that there's no compatible snapshot
* Modify version bounds in your .cabal file to be compatible with the selected snapshot
* Add `extra-deps` to your stack.yaml file to fix compatibility problems

Remember that running `stack build` will give you information on why your build
cannot occur, which should help guide you through the steps necessary for the
second and third option above. Also, note that those options can be
mixed-and-matched, e.g. you may decide to relax some version bounds in your
.cabal file, while also adding some extra-deps.

### Explicit breakage

As mentioned above, updating your list of packages will not cause stack to
invalidate any existing package databases. That's because stack is always
explicit about build plans, via:

1. the selected snapshot
2. the extra-deps
3. local packages

The only way to change a plan for packages to be installed is by modifying one
of the above. This means that breakage of a set of installed packages is an
*explicit* and *contained* activity. Specifically, you get the following
guarantees:

* Since snapshots are immutable, the snapshot package database will not be invalidated by any action. If you change the snapshot you're using, however, you may need to build those packages from scratch.
* If you modify your extra-deps, stack may need to unregister and reinstall them.
* Any changes to your local packages trigger a rebuild of that package and its dependencies.
