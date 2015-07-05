## The Haskell Tool Stack

[![Build Status](https://travis-ci.org/commercialhaskell/stack.svg?branch=master)](https://travis-ci.org/commercialhaskell/stack)
![Release](https://img.shields.io/github/release/commercialhaskell/stack.svg)

`stack` is a cross-platform program for developing Haskell
projects. It is aimed at Haskellers both new and experienced.

<img src="http://i.imgur.com/WW69oTj.gif" width="50%" align="right">

It features:

* Installing GHC automatically.
* Installing packages needed for your project.
* Building your project.
* Testing your project.
* Benchmarking your project.

#### How to install

Downloads are available by operating system:

* [Windows](https://github.com/commercialhaskell/stack/wiki/Downloads#windows)
* [OS X](https://github.com/commercialhaskell/stack/wiki/Downloads#os-x)
* [Ubuntu](https://github.com/commercialhaskell/stack/wiki/Downloads#ubuntu)
* [Debian](https://github.com/commercialhaskell/stack/wiki/Downloads#debian)
* [CentOS / Red Hat](https://github.com/commercialhaskell/stack/wiki/Downloads#centos--red-hat)
* [Fedora](https://github.com/commercialhaskell/stack/wiki/Downloads#fedora)
* [Arch Linux](https://github.com/commercialhaskell/stack/wiki/Downloads#arch-linux)
* [Linux (general)](https://github.com/commercialhaskell/stack/wiki/Downloads#linux)

#### How to use

Go into a Haskell project directory and run `stack build`. If everything is
already configured, this will:

* Download the package index.
* Download and install all necessary dependencies for the project.
* Build and install the project.

You may be prompted to run some of the following along the way:

* `stack new` to create a brand new project.
* `stack init` to create a stack configuration file for an existing project.
  stack will figure out what Stackage release (LTS or nightly) is appropriate
  for the dependencies.
* `stack setup` to download and install the correct GHC version. (For
  information on installation paths, please use the `stack path` command.)

If you just want to install a package using stack, then all you have
to do is `stack install <package-name>`.

Run `stack` for a complete list of commands.

#### Architecture

A full description of the architecture
[is available here](https://github.com/commercialhaskell/stack/wiki/Architecture).

#### Questions, Feedback, Discussion

* For frequently asked questions about detailed or specific use-cases,
  please see
  [the FAQ](https://github.com/commercialhaskell/stack/wiki/FAQ).
* For general questions, comments, feedback and support please write
  to [the stack mailing list](https://groups.google.com/d/forum/haskell-stack).
* For bugs, issues, or requests please
  [open an issue](https://github.com/commercialhaskell/stack/issues/new).

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
