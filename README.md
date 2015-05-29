## The Haskell Tool Stack

stack is a build tool for Haskell code. It handles installation and management
of dependencies, builds multi-package projects, isolates package installations
from each other, and will install build tool dependencies for you (including
GHC). It is designed from the ground up with sensible defaults and a
user-friendly interface.

For more details on how stack works internally, please see [the architecture
document](ARCHITECTURE.md). **FIXME more correct link once moved to final repo
location**

### Usage

1. Download stack following the instructions at **FIXME** and place it on your `PATH`
2. Run `stack build` from within your project to generate a config file, install dependencies, and build your code
    * NOTE: if necessary, run `stack setup` to download GHC and then run `stack build` again. stack will tell you if this is required

Running `stack` will give you a list of commands, some of the most common are:

* `stack test` will build and run your test suites
* `stack bench` will build abd run your benchmarks
* `stack exec` will run a command with a modified environment
    * For convenience, we also provide `stack ghc`, `stack runghc`, and `stack ghci`, which work just like `stack exec`

### Basic information

* stack builds on top of the Cabal library, making it compatible with the existing Haskell package ecosystem and projects.
* Every *project* contains a `stack.yaml` configuration file, which `stack build` will autogenerate for you
* A project can have multiple *packages* if desired
* stack uses [LTS Haskell](https://github.com/fpco/lts-haskell) and [Stackage Nightly](https://github.com/fpco/stackage-nightly) by default to provide for robust dependency resolution

### FAQ

So that this doesn't become repetitive: for the reasons behind the answers
below, see the architecture document. The goal of the answers here is to be as
helpful and concise as possible.

__I need to use a different version of a package than what is provided by the LTS Haskell snapshot I'm using, what should I do?__

You can make tweaks to a snapshot by modifying the `extra-deps` configuration value in your `stack.yaml` file, e.g.:

```yaml
resolver: lts-2.9
packages:
- '.'
extra-deps:
- text-1.2.1.1
```

Note that any packages from the snapshot which depend on the modified package
will not be available to you.

__How do I use this with sandboxes?__

Explicit sandboxing on the part of the user is not required by stack. All
builds are automatically isolated into separate package databases without any
user interaction. This ensures that you won't accidentally corrupt your
installed packages with actions taken in other projects.

__I already have GHC installed, can I still use stack?__

Yes. stack will default to using whatever GHC is on your PATH. If that GHC is a
compatible version with the snapshot you're using, it will simply use it.
Otherwise, you can either:

* Install a different GHC yourself and modify your PATH
* Use `stack setup`

Note that `stack setup` doesn't work for all OSes, so in some cases the first
option will be required. Also, `stack setup` is a no-op when an appropriate GHC
is already available, so it's safe to run it from an automated script, for
example.

__How do I get extra build tools?__

stack will automatically install build tools required by your packages or their
dependencies, in particular alex and happy.

__How does stack choose which snapshot to use when creating a new config file?__

It checks the two most recent LTS Haskell major versions and the most recent
Stackage Nightly for a snapshot that is compatible with all of the version
bounds in your .cabal file, favoring the most recent LTS. For more information,
see the snapshot auto-detection section in the architecture document.

__I'd like to use my installed packages in a different directory. How do I tell stack where to find my packages?__

Set the `STACK_YAML` environment variable to point to the `stack.yaml` config
file for your project. Then you can run `stack exec`, `stack ghc`, etc., from
any directory and still use your packages.

__How do I update my package index?__

Users of cabal are used to running `cabal update` regularly. You can do the
same with stack by running `stack update`. But generally, it's not necessary:
if the package index is missing, or if a snapshot refers to package/version
that isn't available, stack will automatically update and then try again. If
you run into a situation where stack doesn't automatically do the update for
you, please report it as a bug.

__Isn't it dangerous to automatically update the index? Can't that corrupt build plans?__

No, stack is very explicit about which packages it's going to build for you.
There are three sources of information to tell it which packages to install:
the selected snapshot, the `extra-deps` configuration value, and your local
packages. The only way to get stack to change its build plan is to modify one
of those three. Updating the index will have no impact on stack's behavior.
