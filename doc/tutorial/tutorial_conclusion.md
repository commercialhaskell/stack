  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 13. In conclusion

## `stack.yaml` versus Cabal files

Now that we've covered a lot of Stack use cases, this quick summary of
`stack.yaml` versus Cabal files will hopefully make sense and be a good reminder
for future uses of Stack:

* A project can have multiple packages.
* Each project has a `stack.yaml`.
* Each package has a Cabal file, named `<package_name>.cabal`.
* The Cabal file specifies which packages are dependencies.
* The `stack.yaml` file specifies which packages are available to be used.
* The Cabal file specifies the components, modules, and build flags provided by
  a package
* `stack.yaml` can override the flag settings for individual packages
* `stack.yaml` specifies which packages to include

## Comparison to other tools

Stack is not the only tool available for building Haskell code. Stack came into
existence due to limitations at that time with some of the existing tools. If
you are happily building Haskell code with other tools, you may not need Stack.
If you're experiencing problems with other tools, give Stack a try instead.

If you're a new user who has no experience with other tools, we recommend Stack.
The defaults match modern best practices in Haskell development, and there are
fewer corner cases you need to be aware of. You *can* develop Haskell code with
other tools, but you probably want to spend your time writing code, not
convincing a tool to do what you want.

### Underlying package format

Before turning to differences, we clarify an important similarity: Stack, Cabal
(the tool), and presumably all other tools share the same underlying package
format of Cabal (the library). This is a Good Thing: we can share the same set
of upstream libraries, and collaboratively work on the same project with Stack,
Cabal (the tool), and NixOS. In that sense, we're sharing the same ecosystem.

### Curation vs dependency solving

* Stack uses 'curation' (snapshots and Stack's project-level configuration file
  (`stack.yaml`, by default) define precisely the set of packages available for
  a project). The Stack team firmly believes that the majority of users want to
  simply ignore dependency resolution nightmares and get a valid build plan from
  day one. That's why we've made 'curation' the focus of Stack.

* Cabal (the tool) can use 'curation' too but its origins are in dependency
  solving.

### Emphasis on reproducibility

* Stack goes to great lengths to ensure that `stack build` today does the
  same thing tomorrow. With Stack, changing the build plan is always an explicit
  decision.

* Cabal (the tool) does not go to the same lengths: build plans can be affected
  by the presence of pre-installed packages, and running `cabal update` can
  cause a previously successful build to fail.

### Automatic building of dependencies

*   Stack's automatically builds dependencies. So for example, in Stack,
    `stack test` does the same job as:

    ~~~text
    cabal install --enable-tests --only-dependencies
    cabal configure --enable-tests
    cabal build
    cabal test
    ~~~

    (newer versions of Cabal (the tool) may make this command sequence shorter).

*   With Cabal (the tool), you need to use `cabal install` to trigger dependency
    building. This is somewhat necessary as building dependencies can, in some
    cases, break existing installed packages.

### Isolation

* Stack is isolated - provides 'sandboxed' behaviour - by default, via its
  databases. In other words: when you use Stack, there's
  __no need for sandboxes__, everything is (essentially) sandboxed by default.

* With Cabal (the tool), the default behavior is a non-isolated build where
  working on two projects can cause the user package database to become
  corrupted. The Cabal solution to this is sandboxes.

### Tools other than Stack and Cabal (the tool)

* [cabal-meta](https://hackage.haskell.org/package/cabal-meta) inspired a lot of
  the multi-package functionality of Stack. Still relevant for Cabal (the
  tool).
* [cabal-src](https://hackage.haskell.org/package/cabal-src). Deprecated in
  favor of Stack in 2016.
* [stackage-cli](https://hackage.haskell.org/package/stackage-cli).Deprecated
  in favor of Stack in 2015.
* [cabal-dev](https://hackage.haskell.org/package/cabal-dev). Deprecated in
  favor of Cabal (the tool) in 2013.
