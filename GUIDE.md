stack is a cross-platform programm for developing Haskell projects. This guide
is intended to step a new stack user through all of the typical stack
workflows. This guide will not teach you Haskell, but will also not be looking
at much code. This guide will not presume prior experience with the Haskell
packaging system or other build tools.

## What is stack?

* Reproducible builds
* Isolated: don't be afraid of running commands here, everything goes on in stack-specific directories and does *not* affect existing Haskell installations

_NOTE_ In this guide, I'll be running commands on a Linux system (Ubuntu 14.04,
64-bit) and sharing output from there. Output on other systems- or with
different versions of stack- will be slightly different. But all commands work
in a cross platform way, unless explicitly stated otherwise.

## Downloading

## Hello World

* stack new helloworld new-template
* stack build (oh, I need GHC)
* stack setup (downloading... unpacking... installing...)
* stack build (hooray!)
* stack exec helloworld-exe
* stack test

## The setup command

* stack setup (nothing happens)

## The build command

## Adding dependencies

* To cabal file
* Outside of package set

## Curated package sets

* stackage.org
    * Resolver names
    * Hoogle

## Resolvers and changing your compiler version

## Different databases

## The build synonyms

* build, test, bench, haddock, and install
* Building executables (alex, happy, warp, etc)

## Flags and GHC options

## Targets, locals, and extra-deps

* https://github.com/commercialhaskell/stack/wiki/Build-command
* Upstream packages from Github and HTTPS

## path

* How to completely remove traces of stack

## exec

## repl

## ghc/runghc

## IDE

## Templates

## stack.yaml details

* Link to [Wiki page](https://github.com/commercialhaskell/stack/wiki/stack.yaml)

## stack.yaml vs .cabal files

## Comparison to other tools

* Curation vs dependency solving as a default
* Automatically building dependencies
* Reproducible
* Isolated by default (no need for sandboxes!)

## More resources

* `stack --help`
* `stack --version`
* `--verbose` (or `-v`) flag
* Mailing list/Stack Overflow/Wiki

## Fun features

### stack dot

### Travis with caching

### Script interpreter

### Docker

* Don't forget creating images!

### Shell autocompletion

### Custom snapshots

## Power user commands

* update
* unpack
* sdist
* upload
* upgrade
* list-dependencies
