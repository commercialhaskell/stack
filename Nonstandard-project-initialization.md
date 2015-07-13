# Introduction
The purpose of this page is to collect information about issues that arise when users either have an existing cabal project or another nonstandard setup such as a private hackage database. 

# Passing flags to cabal

Any build command, `bench`, `install`, `haddock`, `test`, etc. takes a `--flag` option which passes flags to cabal. Another way to do this is using the flags field in a `stack.yaml`, with the option to specify flags on a per project basis. 

As an example, in a `stack.yaml` for multi-package project with packages `foo`, `bar`, `baz`:

```
flags:
  foo:
    release: true
  bar:
    default: true
  baz:
    manual: true
```

It is also possible to pass the same flag to multiple packages, i.e. `stack build --flag *:necessary`

Currently one needs to list all of your modules that interpret flags in the `other-modules` section of a cabal file. `cabal-install` has a different behavior currently and doesn't require that the modules be listed. This may change in a future release. 


## Issues Referenced
https://github.com/commercialhaskell/stack/issues/191
https://github.com/commercialhaskell/stack/issues/417
https://github.com/commercialhaskell/stack/issues/335
https://github.com/commercialhaskell/stack/issues/301
https://github.com/commercialhaskell/stack/issues/365
https://github.com/commercialhaskell/stack/issues/105

# Selecting a resolver

`stack init` or `stack new` will try to default to the current Haskell LTS present on `https://www.stackage.org/snapshots`. Using an incorrect resolver can cause a build to fail if the version of GHC it requires is not present. In order to override the resolver entry at project initialization one can pass the `--resolver` option with the name of any snapshots on Stackage. Alternatively `--prefer-lts` and `--prefer-nightly` will choose a current lts or nightly versions accordingly. 

:TODO: Document `--solver`

## Issues Referenced
https://github.com/commercialhaskell/stack/issues/468
https://github.com/commercialhaskell/stack/issues/464

# Using git repos
stack has support for packages that reside in remote git locations.

Example,

```
- location:
    git: https://github.com/kolmodin/binary
    commit: 8debedd3fcb6525ac0d7de2dd49217dce2abc0d9
```

https://github.com/commercialhaskell/stack/issues/254
https://github.com/commercialhaskell/stack/issues/199
# Private Hackage
https://github.com/commercialhaskell/stack/issues/445

# Custom Snapshots
https://github.com/commercialhaskell/stack/issues/111

# intra-package targets
 https://github.com/commercialhaskell/stack/issues/201

## Common Gotchas 
Add an example of how to add a dependency to a project to the README and/or Wiki 
 https://github.com/commercialhaskell/stack/issues/510
