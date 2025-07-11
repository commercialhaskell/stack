---
title: Getting started
---

  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Getting started

Stack is a program for developing [Haskell](https://www.haskell.org/) projects.

This guide to getting started takes a new Stack user through the ways that Stack
is typically used. It will not teach Haskell or involve much code, and it
requires no prior experience of Stack or other Haskell tools.

Terms used in the guide will be explained as they are introduced and are also
defined in the [glossary](../glossary.md).

Some of Stack's features will not be needed regularly or by all users. Other
parts of Stack's documentation include its [commands](../commands/index.md) and
its [configuration](../configure/index.md).

## Other Haskell tools

First, simplifying greatly, let us briefly place Stack and other Haskell tools
in the Haskell landscape.

Haskell was specified in the
[Haskell 98 Language and Library Reports](https://www.haskell.org/onlinereport/),
first published in February 1999, and further specified in the
[Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/),
published in April 2010. Extensions to the language have been developed (see
below).

The Haskell compiler is **GHC** (the
[Glasgow Haskell Compiler](https://www.haskell.org/ghc/)). It can compile
Haskell code into executable and other binary files. GHC can also be used
interactively (**GHCi**) and Stack supports such use.

~~~mermaid
flowchart LR
  code@{ shape: docs, label: "Haskell code" }
  ghc[GHC]
  binaries@{ shape: docs, label: "Executable and other binary files" }
  code --> ghc --> binaries
~~~

A lot of interesting things can be done using only what comes with GHC
(including GHCi) but as Haskell code becomes more complex and there is a desire
to reuse efficiently code developed by others it becomes necessary to organise
code in a structured way.

In that regard, Haskell code can be organised into *packages*. A Haskell package
also includes a file that describes the package's contents. The most established
description format is known as a
[Cabal file](https://cabal.readthedocs.io/en/stable/file-format-changelog.html).
The [**Hpack** project](https://github.com/sol/hpack) provides a modern
alternative format (in a file named `package.yaml`) and a library and
application that translates from that format to the legacy format. Stack has
built-in support for Hpack. Other build tools (see below) do not.

~~~mermaid
flowchart LR
  packageYaml@{ shape: doc, label: "package.yaml" }
  hpack[Hpack]
  cabalFile@{ shape: doc, label: "Cabal file" }
  packageYaml --> hpack --> cabalFile
~~~

The code in a Haskell package is organised into *components*, including
components known as *libraries*. Historically, a package had no more than one
library component. The Cabal specification has developed to allow a package to
have named *sub-library* components as well as a main library.

The code in a Haskell package can depend on the libraries in the same package
or in another package. These are known as its *dependencies*.

GHC comes with the libraries of certain Haskell packages (known as
*boot packages*) already installed in its global database of installed
libraries. These include the library of the
[package `base`](https://hackage.haskell.org/package/base), which
is a dependency of almost all other packages, and the library of
[package `Cabal`](https://hackage.haskell.org/package/Cabal), which provides
code to build packages and components of packages using GHC.

Two important public databases are **Hackage**
[(the  Haskell Package Repository)](https://hackage.haskell.org/) and
[**Stackage**](https://hackage.haskell.org/).

~~~mermaid
flowchart LR
  hackage@{ shape: lin-cyl, label: "Hackage
    (packages)" }
  stackage@{ shape: lin-cyl, label: "Stackage
    (snapshots)" }
  hackage --> stackage
~~~

Hackage is a database of Haskell packages, each identified by a name and version
number. There were over 17,000 package names on Hackage as at July 2025.

Stackage is a database of collections of Haskell package versions on
Hackage that are known, by testing, to work well together and with a specific
version of GHC and its boot packages. Those collections are known as
*snapshots*. As at July 2025, a snapshot for a recent version of GHC includes
well over 3,000 packages.

Stack can unpack packages from, and upload packages to, Hackage and
builds making use of snapshots from Stackage.

GHC comes with an application
[**Haddock**](https://haskell-haddock.readthedocs.io/latest/) that automatically
generates web page and other documentation from annotated Haskell code. The
Hackage and Stackage websites display that documentation. Stack supports the use
of Haddock.

~~~mermaid
flowchart LR
  code@{ shape: docs, label: "Haskell code" }
  haddock[Haddock]
  docs@{ shape: docs, label: "Documentation" }
  code --> haddock --> docs
~~~

**Hoogle** is a [website](https://hoogle.haskell.org/) and an
[application](https://hackage.haskell.org/package/hoogle) that allows its users
to search for the library components of Haskell packages, the modules they
expose, and functions and types exported by modules. Stack supports the use of
Hoogle on the command line.

GHC is described in its
[User Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/). It can
be used directly but it is a complex application with many flags and options.
These include flags to specify extensions to the Haskell language.  Haskell
*build tools* are applications that make it easier to use GHC, including by
applying sensible defaults. Stack is such a build tool. Stack itself uses
the Cabal library to build.

~~~mermaid
flowchart LR
  stack[Stack]
  cabal["Cabal (the library)"]
  code@{ shape: docs, label: "Haskell code" }
  ghc[GHC]
  binaries@{ shape: docs, label: "Executable and other files" }
  subgraph buildtool ["Build tool"]
      direction TB
      stack --> cabal --> ghc
  end
  code --> buildtool --> binaries
~~~

When Haskell code changes, GHC and build tools aim to minimise what needs to be
re-compiled.

A Stack project may comprise only a single package, but Stack can also handle
multi-package projects.

Another build tool is **Cabal** (the tool) (named after the library). It is
provided by the
[`cabal-install`](https://hackage.haskell.org/package/cabal-install)
Haskell package. Stack can be used independently of Cabal (the tool) but users
can also use both, if they wish.

Some popular code editors (including
[Visual Studio Code](https://code.visualstudio.com/)) have extensions that
support Haskell coding by providing an integrated development environment (IDE).
Those extensions use **HLS** (the
[Haskell Language Server](https://haskell-language-server.readthedocs.io/en/stable/)),
an application that implements the Language Server Protocol for Haskell.

Stack can manage versions of GHC and upgrade/downgrade Stack itself and is, in
that sense, an *installer* of those applications as well as a build tool.
However, Cabal (the tool) is not an installer and versions of HLS applicable to
versions of GHC also need to be installed.
[**GHCup**](https://www.haskell.org/ghcup/) is an installer of versions of GHC,
HLS, Stack and Cabal (the tool) built for various operating systems and machine
architectures. Stack can be configured to manage versions of GHC by using GHCup.

~~~mermaid
flowchart TD
  ghcUp["GHCup
    installer"]
  ghc["GHC
    compiler"]
  hls["HLS
    IDE tool"]
  stack["Stack
    build tool, installer"]
  cabal["Cabal (the tool)
    build tool"]
  ghcUp -.-> ghc
  ghcUp -.-> hls
  ghcUp -.-> stack
  stack -- "(optional) to fetch GHC" --> ghcUp
  ghcUp -.-> cabal
~~~

## Setting up

The goal of setting up is a `stack` executable on the PATH. As we will see, when
Stack is used, it sets other things up as needed.

*[PATH]: An environment variable that specifies a list of directories searched for executable files.

For further information about setting up, see the
[documentation](../install_and_upgrade.md) on that topic. Return here when you
know that Stack is on the PATH.

This guide assumes that the directory where Stack install executables (the
location reported by `stack path --local-bin`) has been added to the PATH.

This guide assumes that your computer's operating system is one of Linux, macOS
or Windows. Stack's commands are the same on all operating systems.
