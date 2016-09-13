# Non-standard project initialization

## Introduction
The purpose of this page is to collect information about issues that arise when
users either have an existing cabal project or another nonstandard setup such
as a private hackage database.

## Using a Cabal File New users may be confused by the fact that you must add
dependencies to the package's cabal file, even in the case when you have
already listed the package in the `stack.yaml`. In most cases, dependencies for
your package that are in the Stackage snapshot need *only* be added to the
cabal file. stack makes heavy use of Cabal the library under the hood. In
general, your stack packages should also end up being valid cabal-install
packages.

### Issues Referenced
  - https://github.com/commercialhaskell/stack/issues/105

## Passing Flags to Cabal

Any build command, `bench`, `install`, `haddock`, `test`, etc. takes a `--flag`
option which passes flags to cabal. Another way to do this is using the flags
field in a `stack.yaml`, with the option to specify flags on a per package
basis.

As an example, in a `stack.yaml` for multi-package project with packages `foo`,
`bar`, `baz`:

```
flags:
  foo:
    release: true
  bar:
    default: true
  baz:
    manual: true
```

It is also possible to pass the same flag to multiple packages, i.e. `stack
build --flag *:necessary`

Currently one needs to list all of your modules that interpret flags in the
`other-modules` section of a cabal file. `cabal-install` has a different
behavior currently and doesn't require that the modules be listed. This may
change in a future release.


### Issues Referenced
  - https://github.com/commercialhaskell/stack/issues/191
  - https://github.com/commercialhaskell/stack/issues/417
  - https://github.com/commercialhaskell/stack/issues/335
  - https://github.com/commercialhaskell/stack/issues/301
  - https://github.com/commercialhaskell/stack/issues/365
  - https://github.com/commercialhaskell/stack/issues/105

## Selecting a Resolver

`stack init` or `stack new` will try to default to the current Haskell LTS
present on `https://www.stackage.org/snapshots` if no snapshot has been
previously used locally, and to the latest LTS snapshot locally used for a
build otherwise. Using an incorrect resolver can cause a build to fail if the
version of GHC it requires is not present.

In order to override the resolver entry at project initialization one can pass
`--prefer-lts` or `--prefer-nightly`. These options will choose the latest LTS
or nightly versions locally used.  Alternatively the `--resolver` option can be
used with the name of any snapshots on Stackage, or with `lts` or `nightly` to
select the latest versions, disregarding previously used ones. This is not the
default so as to avoid unnecessary recompilation time.

:TODO: Document `--solver`

### Issues Referenced
  - https://github.com/commercialhaskell/stack/issues/468
  - https://github.com/commercialhaskell/stack/issues/464

## Using git Repositories
stack has support for packages that reside in remote git locations.

Example:

```
packages:
- '.'
- location:
    git: https://github.com/kolmodin/binary
    commit: 8debedd3fcb6525ac0d7de2dd49217dce2abc0d9
```

### Issues Referenced
  - https://github.com/commercialhaskell/stack/issues/254
  - https://github.com/commercialhaskell/stack/issues/199

## Private Hackage
Working with a private Hackage is currently supported in certain situations.
There exist special entries in `stack.yaml` that may help you. In a
`stack.yaml` file, it is possible to add lines for packages in your database
referencing the sdist locations via an `http` entry, or to use a `Hackage`
entry.

The recommended stack workflow is to use git submodules instead of a private
Hackage. Either by using git submodules and listing the directories in the
packages section of `stack.yaml`, or by adding the private dependencies as git
URIs with a commit SHA to the `stack.yaml`. This has the large benefit of
eliminating the need to manage a Hackage database and pointless version bumps.

For further information see [YAML configuration](yaml_configuration.md)

### Issues Referenced
  - https://github.com/commercialhaskell/stack/issues/445
  - https://github.com/commercialhaskell/stack/issues/565

## Custom Snapshots
Currently WIP?
### Issues Referenced
  - https://github.com/commercialhaskell/stack/issues/111
  - https://github.com/commercialhaskell/stack/issues/253
  - https://github.com/commercialhaskell/stack/issues/137

## Intra-package Targets
stack supports intra-package targets, similar to `cabal build COMPONENTS` for
situations when you don't want to build every target inside your package.

Example:
```
stack build stack:lib:stack
stack test stack:test:stack-integration-test
```

Note: this does require prefixing the component name with the package name.

### Issues referenced
  - https://github.com/commercialhaskell/stack/issues/201
