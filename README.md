## The Haskell Tool Stack

> ᕕ( ᐛ )ᕗ I'm off to install stack 

`stack` is an easy and reliable way to build your Haskell projects. It works with
simple one-file executables all the way up to massive, multi-package projects with modified
local dependencies. It uses sensible defaults- like isolation binary
installations and LTS Haskell- while allowing full user customization. All
builds are reproducible: changes in the upstream package repository or
work you do on other projects will not break your codebase. If your code builds
today, it will build tomorrow.

stack is a full stack build tool, capable of installing the appropriate version of
GHC for you automatically and building and registering your upstream library
dependencies into shared package databases (to minimize compilation time). It
will intelligently recompile your libraries as necessary to enable profiling
support, and give helpful error messages and advice for fixing your project.

`stack` has been designed from the ground up to be as easy as
possible to use. In most cases, you'll be able to get your project building
by just running `stack build`.

For more details on how stack works internally, please see [the architecture
document](ARCHITECTURE.md). **FIXME more correct link once moved to final repo
location** For more motivation for the tool, see "Why stack" below.

### Usage

1. Download stack following the install instructions below.
2. Run `stack build` from within your project to generate a config file, install dependencies, and build your code

Running `stack` will give you a list of commands, some of the most common are:

* `stack test` will build and run your test suites
* `stack bench` will build and run your benchmarks
* `stack exec` will run a command with a modified environment
    * For convenience, we also provide `stack ghc`, `stack runghc`, and `stack ghci`, which work just like `stack exec`

### Install

Distribution packages are available for Ubuntu and Arch Linux. For the future, we have plans to support more OSes.

#### Ubuntu

* Ubuntu 15.04 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/vivid stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 14.10 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/utopic stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 14.04 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 12.04 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/precise stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```
#### Arch Linux

stack can be found in the AUR:
  - [haskell-stack](https://aur.archlinux.org/packages/haskell-stack/) _latest stable version_
  - [haskell-stack-git](https://aur.archlinux.org/packages/haskell-stack-git/) _git version_

### Basic information

* stack builds on top of the Cabal library, making it compatible with the existing Haskell package ecosystem and projects.
* Every *project* contains a `stack.yaml` configuration file, which `stack build` will autogenerate for you
* A project can have multiple *packages* if desired
* stack uses [LTS Haskell](https://github.com/fpco/lts-haskell) and [Stackage Nightly](https://github.com/fpco/stackage-nightly) by default to provide for robust dependency resolution

The stack.yaml file is pretty self-explanatory, here's a real-life example from [the WAI project](https://github.com/yesodweb/wai):

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
```

Here's a list of open source stack.yaml files for reference and examples of
real-world usage:

* [Roman Cheplyaka's prover](https://github.com/feuerbach/prover/blob/master/stack.yaml)
* [Web Application Interface](https://github.com/yesodweb/wai/blob/master/stack.yaml)

If you have a stack.yaml file which you believe would be useful for others to
see, please send a pull request to this file to include a link.

### Why stack?

stack is a project of the [Commercial Haskell](http://commercialhaskell.com/)
group, spearheaded by [FP Complete](https://www.fpcomplete.com/). It is
designed to answer the needs of commercial Haskell users, hobbyist Haskellers,
and individuals and companies thinking about starting to use Haskell. It is
intended to be easy to use for newcomers, while providing the customizability
and power experienced developers need.

While stack itself has been around since June of 2015, it is based on codebases
used by FP Complete for its corporate customers and internally for years prior.
stack is a refresh of that codebase combined with other open source efforts
like [stackage-cli](https://github.com/fpco/stackage-cli) to meet the needs of
users everywhere.

A large impetus for the work on stack was a [large survey of people interested
in
Haskell](https://www.fpcomplete.com/blog/2015/05/thousand-user-haskell-survey),
which rated build issues as a major concern. The stack team hopes that stack
can address these concerns.

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

The above example specifies that the `proprietary-dep` package is found in the project's `third-party` folder, that the `conduit` package is found in the project's `github-version-of` folder, and that the `diagrams` package is found in the project's `patched` folder.

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

__Can I get bash autocomplation?__

Yes, just run the following:

    source <(stack --bash-completion-script `which stack`)

You can add this to your `.bashrc` file if you like.

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

You can configure this in your stack.yaml. Here's what the default configuration looks like:

```yaml
package-indices:
- name: hackage.haskell.org
  download-prefix: https://s3.amazonaws.com/hackage.fpcomplete.com/package/

  # at least one of the following must be present
  git: https://github.com/commercialhaskell/all-cabal-hashes.git
  http: https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz

  # optional fields, both default to false
  gpg-verify: false
  require-hashes: false
```

One thing you should be aware of: if you change the contents of package-version
combination by setting a different package index, this *can* have an effect on
other projects by installing into your shared snapshot database.

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

Yes, stack supports using Docker with images that contain all of Stackage and
the tools pre-installed.  Commands like `stack build` will automatically run
in a temporary container (with your project bind-mounted into the container).
To enable, add this to your `stack.yaml`:

```
docker:
  enable: true
```

This will use a Docker image tagged with the same LTS version as your resolver.
Note that the Docker images are really big (approx. 10 GB), and not all LTS
versions have a Docker image (they tend to lag behind).  We also recommend
against using this with Docker's _devicemapper_ storage driver, since its
default settings have trouble with images this big.
