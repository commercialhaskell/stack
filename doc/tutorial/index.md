---
Title: Getting started
---

  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Getting started

Stack is a program for developing [Haskell](https://www.haskell.org/) projects.

This guide to getting started takes a new Stack user through the ways that Stack
is typically used. It will not teach Haskell or involve much code, and it
requires no prior experience of Stack or other Haskell tools.

??? question "What are other Haskell tools?"

    Haskell code is compiled by the
    [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC), which can
    also be used interactively. Stack can manage versions of GHC. GHC provides
    commands such as `ghc`, `ghci`, `runghc` and `ghc-pkg`.

    Cabal (the tool) is a tool provided by the
    [`cabal-install`](https://hackage.haskell.org/package/cabal-install) Haskell
    package. It aims to simplify the process of managing Haskell software by
    automating the fetching, configuration, compilation and installation of
    Haskell libraries and programs. These are goals that Stack shares. Stack can
    be used independently of Cabal (the tool) but users can also use both, if
    they wish.

    Haskell Language Server (HLS) is an implementation of the Language Server
    Protocol for Haskell and used by Haskell extensions for code editors.

    [GHCup](https://www.haskell.org/ghcup/) is a tool that can manage other
    Haskell tools, including Stack, GHC, HLS and Cabal (the tool). Stack can use
    GHCup to manage versions of GHC, as well as manage GHC directly.

Terms used in the guide will be explained as they are introduced and are also
defined in the [glossary](../glossary.md).

Some of Stack's features will not be needed regularly or by all users. Other
parts of Stack's documentation include its [commands](../commands/index.md) and
its [configuration](../configure/index.md).

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
