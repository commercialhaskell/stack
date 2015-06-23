So that this doesn't become repetitive: for the reasons behind the answers
below, see the [[Architecture]] page. The goal of the answers here is to be as
helpful and concise as possible.

__Where is stack installed and will it interfere with `ghc` (etc) I already have installed?__

Stack is installed under your `.stack` directory in your home directory. It
should not affect your existing installation at all.

__What is the relationship between stack and cabal?__

* Cabal-the-library is used by stack to build your Haskell code.
* cabal-install (the executable) is not used at all by stack.
* A .cabal file is provided for each package, and defines all package-level metadata just like it does in the cabal-install world: modules, executables, test suites, etc. No change at all on this front.
* A stack.yaml file references 1 or more packages, and provides information on where dependencies come from.
* `stack build` currently initializes a stack.yaml from the existing .cabal file. Project initialization is something that is still being discussed and there may be more options here for new projects in the future (see issue [253](https://github.com/commercialhaskell/stack/issues/253))

__I need to use a different version of a package than what is provided by the LTS Haskell snapshot I'm using, what should I do?__

You can make tweaks to a snapshot by modifying the `extra-deps` configuration value in your `stack.yaml` file, e.g.:

```yaml
resolver: lts-2.9
packages:
- '.'
extra-deps:
- text-1.2.1.1
```

__I need to use a package (or version of a package) that is not available on hackage, what should I do?__

Add it to the `packages` list in your project's `stack.yaml`, specifying the package's source code location relative to the directory where your `stack.yaml` file lives, e.g.

```yaml
resolver: lts-2.10
packages:
- '.'
- third-party/proprietary-dep
- github-version-of/conduit
- patched/diagrams
extra-deps: []
```

The above example specifies that the `proprietary-dep` package is found in the project's `third-party` folder, that the `conduit` package is found in the project's `github-version-of` folder, and that the `diagrams` package is found in the project's `patched` folder. This autodetects changes and reinstalls the package.

__What is the meaning of the arguments given to stack build, test, etc?__

Those are the targets of the build, and can have one of three formats:

* A package name (e.g., `my-package`) will mean that the `my-package` package must be built
* A package identifier (e.g., `my-package-1.2.3`), which includes a specific version. This is useful for passing to `stack install` for getting a specific version from upstream
* A directory (e.g., `./my-package`) for including a local directory's package, including any packages in subdirectories

__I need to modify an upstream package, how should I do it?__

Typically, you will want to get the source for the package and then add it to
your `packages` list in stack.yaml. (See the previous question.)
`stack unpack` is one approach for getting the source.
Another would be to add the upstream package as a submodule to your
project.


__How do I use this with sandboxes?__

Explicit sandboxing on the part of the user is not required by stack. All
builds are automatically isolated into separate package databases without any
user interaction. This ensures that you won't accidentally corrupt your
installed packages with actions taken in other projects.

__I already have GHC installed, can I still use stack?__

Yes. stack will default to using whatever GHC is on your PATH. If that GHC is a
compatible version with the snapshot you're using, it will simply use it.
Otherwise, if will install the new version for you. (If you don't want this
automatic installation, you can use the `--no-install-ghc` option.)

Note that GHC installation doesn't work for all OSes, so in some cases the
first option will need to install GHC yourself. Also, the `stack setup` command
gives you more direct control of installing GHC. When the necessary GHC is
already installed, `stack setup` is a no-op, so it's safe to run it from an
automated script, for example.

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

__Can I get bash autocompletion?__

Yes, see the [Shell-autocompletion](https://github.com/commercialhaskell/stack/wiki/Shell-autocompletion) wiki entry

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

__I have a custom package index I'd like to use, how do I do so?__

You can configure this in your stack.yaml. See [[stack.yaml]]

__How can I make sure my project builds against multiple ghc versions?__

You can create multiple yaml files for your project,
one for each build plan. For example, you might set up your project directory like so:

```
myproject/
  stack-7.8.yaml
  stack-7.10.yaml
  stack.yaml --> symlink to stack-7.8.yaml
  myproject.cabal
  src/
    ...
```

When you run `stack build`, you can set the
`STACK_YAML` environment variable to indicate which build plan to use.

```
$ stack build                             # builds using the default stack.yaml
$ STACK_YAML=stack-7.10.yaml stack build  # builds using the given yaml file
```

__I heard you can use this with Docker?__

Yes, stack supports using Docker with images that contain preinstalled Stackage
packages and the tools. See [[Docker]] for details.

__How do I use this with Travis CI?__

Stack is in beta now, so keep in mind that the tool's interface is still under flux. For early adopters, here's a sample `.travis.yaml` file:

```YAML
language: haskell

before_install:
  # Instructions taken from https://github.com/commercialhaskell/stack/wiki/Downloads
  - wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
  - echo 'deb http://download.fpcomplete.com/ubuntu/precise stable main' | sudo tee /etc/apt/sources.list.d/fpco.list
  - sudo apt-get update
  - sudo apt-get install stack -y

install:
  - stack setup

script:
  - stack test
```

If you wish to use stack as part of a larger matrix, à la [hvr/multi-ghc-travis](https://github.com/hvr/multi-ghc-travis), then you need to do a bit more work. Take a look at [tebello-thejane/bitx-haskell/.travis.yml](https://github.com/tebello-thejane/bitx-haskell/blob/master/.travis.yml), and note the extensive use of `if` statements to select between building with Cabal and stack.

Also, note that `stack setup` has been known to take longer than 20 minutes when cloning the Hackage package index on Travis. Spawning a simple minute counter while using stack is a cheap trick to ensure that the Travis build does not time out due to lack of output for longer than 10 minutes (and that the stack output is instantaneous, unlike using `travis_wait`). See [tebello-thejane/bitx-haskell/.travis/stack-build.sh](https://github.com/tebello-thejane/bitx-haskell/blob/master/.travis/stack-build.sh) as an example of how to accomplish this.
