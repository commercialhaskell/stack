## The Haskell Tool Stack

[![Build Status](https://travis-ci.org/commercialhaskell/stack.svg?branch=master)](https://travis-ci.org/commercialhaskell/stack)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/commercialhaskell/stack)](https://ci.appveyor.com/project/snoyberg/stack)
[![Release](https://img.shields.io/github/release/commercialhaskell/stack.svg)](https://github.com/commercialhaskell/stack/releases)

`stack` is a cross-platform program for developing Haskell
projects. It is aimed at Haskellers both new and experienced.

<img src="http://i.imgur.com/WW69oTj.gif" width="50%" align="right">

It features:

* Installing GHC automatically, in an isolated location.
* Installing packages needed for your project.
* Building your project.
* Testing your project.
* Benchmarking your project.

#### How to install

Downloads are available by operating system:

* [Windows](doc/install_and_upgrade.md#windows)
* [OS X](doc/install_and_upgrade.md#os-x)
* [Ubuntu](doc/install_and_upgrade.md#ubuntu)
* [Debian](doc/install_and_upgrade.md#debian)
* [CentOS / Red Hat / Amazon Linux](doc/install_and_upgrade.md#centos--red-hat--amazon-linux)
* [Fedora](doc/install_and_upgrade.md#fedora)
* [Arch Linux](doc/install_and_upgrade.md#arch-linux)
* [NixOS](doc/install_and_upgrade.md#nixos)
* [Linux (general)](doc/install_and_upgrade.md#linux)

[Upgrade instructions](doc/install_and_upgrade.md#upgrade)

Note: if you are using cabal-install to install stack, you may need to pass a
constraint to work around a
[Cabal issue](https://github.com/haskell/cabal/issues/2759): `cabal install
--constraint 'mono-traversable >= 0.9' stack`.

#### Quick Start Guide

First you need to [install it (see previous section)](#how-to-install).

##### Start your new project:

~~~ {.bash}
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe
~~~

- The `stack new` command will create a new directory containing all
the needed files to start a project correctly.
- The `stack setup` will download the compiler if necessary in an isolated
  location (default `~/.stack`) that won't interfere with any system-level
  installations. (For information on installation paths, please use the `stack
  path` command.).
- The `stack build` command will build the minimal project.
- `stack exec my-project-exe` will execute the command.
- If you just want to install an executable using stack, then all you have to do
is`stack install <package-name>`.

If you want to launch a REPL:

~~~ {.bash}
stack ghci
~~~


Run `stack` for a complete list of commands.

##### Workflow

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

#### How to contribute

This assumes that you have already installed a version of stack, and have `git`
installed.

1. Clone `stack` from git with
   `git clone https://github.com/commercialhaskell/stack.git`.
2. Enter into the stack folder with `cd stack`.
3. Build `stack` using a pre-existing `stack` install with
   `stack setup && stack build`.
4. Once `stack` finishes building, check the stack version with
   `stack --version`. Make sure the version is the latest.
5. Look for issues tagged with
   [`newcomer` and `awaiting-pr` labels](https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3Anewcomer+label%3A%22awaiting+pr%22)

Build from source as a one-liner:

```bash
git clone https://github.com/commercialhaskell/stack.git && \
cd stack && \
stack setup && \
stack build
```

#### Complete guide to stack

This repository also contains a complete [user guide to using stack
](doc/GUIDE.md), covering all of the most common use cases.


#### Questions, Feedback, Discussion

* For frequently asked questions about detailed or specific use-cases, please
  see [the FAQ](doc/faq.md).
* For general questions, comments, feedback and support please write
  to [the stack mailing list](https://groups.google.com/d/forum/haskell-stack).
* For bugs, issues, or requests please
  [open an issue](https://github.com/commercialhaskell/stack/issues/new).
* When using Stack Overflow, please use [the haskell-stack
  tag](http://stackoverflow.com/questions/tagged/haskell-stack).

#### Why stack?

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

<hr>

## Documentation Table Of Contents

* Project Documentation
    * [Maintainer Guide](doc/MAINTAINER_GUIDE.md): includes releasing information
    * [Signing Key](doc/SIGNING_KEY.md): downloadable stack binaries are signed
      with this key
* Tool Documentation
    * [Build Command](doc/build_command.md): reference for the syntax of the
      build command and the command line targets
    * [Dependency Visualization](doc/dependency_visualization.md): uses Graphviz
    * [Docker Integration](doc/docker_integration.md)
    * [FAQ](doc/faq.md): frequently asked questions about detailed or specific
      use-cases
    * [Install/Upgrade](doc/install_and_upgrade.md): a list of downloads
      available by operating system, installation instructions, and upgrade
      instructions
    * [Nonstandard Project Initialization](doc/nonstandard_project_init.md)
    * [Shell Autocompletion](doc/shell_autocompletion.md)
    * [User Guide](doc/GUIDE.md): in-depth tutorial covering the most common use
      cases and all major stack features (requires no prior Haskell tooling
      experience)
    * [YAML Configuration](doc/yaml_configuration.md): reference for writing
      `stack.yaml` files
* Advanced Documentation
    * [Architecture](doc/architecture.md): reference for people curious about
      stack internals, wanting to get involved deeply in the codebase, or
      wanting to use stack in unusual ways
