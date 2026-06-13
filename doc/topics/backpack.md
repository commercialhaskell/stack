<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Backpack support

Backpack is an extension to Haskell's module system, supported by GHC since
GHC 8.2.1 and Cabal (the library) since Cabal-2.0.0.2. It lets you write a
library that depends on an abstract interface (a *signature*) instead of a
concrete implementation. The consumer of that library then decides which
implementation to plug in. The compiler recompiles the library for each
implementation, so there is no runtime cost.

Stack supports cross-package Backpack from Stack UNRELASED.

## What Backpack gives you

### Signatures

A *signature file* (`.hsig`) declares the types and functions that an
implementation must provide, without supplying any code. For example, a
signature for an abstract string type:

~~~haskell
-- Str.hsig
signature Str where

data Str

empty  :: Str
append :: Str -> Str -> Str
~~~

The package lists its signatures in the `signatures` field of the library
stanza:

=== "package.yaml"

    ~~~yaml
    library:
      signatures:
      - Str
    ~~~

=== "Cabal file"

    ~~~cabal
    library
      build-depends: base
      signatures:    Str
      exposed-modules: MyModule
    ~~~

Any module inside this library can `import Str` and use it as if it were a
normal module. The compiler will type-check the code against the signature
without needing an actual implementation.

A package that has at least one unfilled signature is called *indefinite*.

### Mixin linking

Backpack fills a signature through *mixin linking*: when a module with the same
name as a signature is brought into scope, the compiler treats that module as
the implementation.

With Stack, you must declare this expressly using a mixin:

=== "package.yaml"

    ~~~yaml
    library:
      dependencies:
      - str-string # exposes module "Str"
      - name: my-indefinite-pkg # has signature "Str"
        mixin:
        - requires (Str as Str)
    ~~~

=== "Cabal file"

    ~~~cabal
    library
      build-depends:
        , base
        , str-string          -- exposes module "Str"
        , my-indefinite-pkg   -- has signature "Str"
      mixins:
        my-indefinite-pkg requires (Str as Str)
    ~~~

!!! note

    Cabal (the library) supports an implicit form of mixin linking (by name,
    without a `mixins` field). However, Stack's instantiation planner only takes
    into account express mixin declarations and a mixin must always be added
    for cross-package Backpack.

### Renaming

When the signature name and the implementation module name differ, a mixin lets
you rename one or the other.

Suppose the indefinite package has signature `Str`, and a separate package
exposes the same declarations under the module name `MyStr`:

=== "package.yaml"

    Rename the requirement to match the implementation:

    ~~~yaml
    dependencies:
    - name: my-indefinite-pkg
      mixin:
      - requires (Str as MyStr)
    ~~~

    or rename the implementation to match the requirement:

    ~~~yaml
    dependencies:
    - name: my-impl-pkg
      mixin:
      - (MyStr as Str)
    ~~~

=== "Cabal file"

    Rename the requirement to match the implementation:

    ~~~cabal
    mixins: my-indefinite-pkg requires (Str as MyStr)
    ~~~

    or rename the implementation to match the requirement:

    ~~~cabal
    mixins: my-impl-pkg (MyStr as Str)
    ~~~

Both achieve the same result: the `Str` signature is filled by `MyStr`.

Note that mixin renaming is module-level: the implementation module must
already export the same identifiers (types, functions) that the signature
declares. Renaming only changes which module name fills which signature; it
does not bridge mismatched contents.

A mixin also supports `hiding` on the `requires` side. This tells the compiler
*not* to fill the listed signatures through mixin linking for this dependency —
they remain as holes and propagate to the consumer.

### Multiple instantiations

You can instantiate the same indefinite package more than once with different
implementations. Each instantiation gets its own renaming as a mixin:

=== "package.yaml"

    ~~~yaml
    dependencies:
    - name: my-indefinite-pkg
      mixin:
      - (MyModule as MyModule.Text) requires (Str as TextStr)
      - (MyModule as MyModule.BS) requires (Str as BSStr)
    ~~~

=== "Cabal file"

    ~~~cabal
    mixins:
      my-indefinite-pkg
        (MyModule as MyModule.Text)
        requires (Str as TextStr),
      my-indefinite-pkg
        (MyModule as MyModule.BS)
        requires (Str as BSStr)
    ~~~

Where `TextStr` and `BSStr` are modules from implementation package dependencies
that export the identifiers the `Str` signature declares. This produces two
copies of `MyModule` — one backed by `TextStr`, the other by `BSStr` —
each with a distinct module name so they do not clash.

### Sublibraries

Backpack projects tend to involve several small libraries (a signature library,
one or more implementation libraries, and a consumer library). A single Haskell
package can include all of these by using sublibraries:

=== "package.yaml"

    ~~~yaml
    spec-version: 0.36.0

    name: myPackage

    dependencies:
    - base

    library:
      dependencies:
      - name: str-sig
        mixin:
        - requires (Str as Str)
      - str-text

    internal-libraries:
      str-sig:
        signatures: Str
      str-text:
        dependencies:
        - text
    ~~~

=== "Cabal file"

    ~~~cabal
    cabal-version: 2.2
    name: myPackage

    library
      build-depends: base, str-sig, str-text
      mixins: str-sig requires (Str as Str)
      exposed-modules: MyModule

    library str-sig
      build-depends: base
      signatures: Str

    library str-text
      build-depends: base, text
      exposed-modules: Str
    ~~~

This is purely an organizational convenience — the semantics are identical to
having three separate packages.

!!! note

    Implementation modules that fill a signature cannot live in the same
    component that has the dependency on the signature package. They must be in
    a separate package or sublibrary.

### Reexported modules

The `reexported-modules` key or field lets you expose an instantiated module
under a public name. This is useful when you want to use Backpack as an internal
implementation detail while presenting a straightforward API to users who do not
need to know about Backpack:

=== "package.yaml"

    ~~~yaml
    library:
      dependencies:
      - name: regex-indef
        mixin:
        - requires (Str as Str)
      - str-bytestring
      reexported-modules:
      - Regex as Regex.ByteString
    ~~~

=== "Cabal file"

    ~~~cabal
    library
      build-depends: base, regex-indef, str-bytestring
      mixins: regex-indef requires (Str as Str)
      reexported-modules: Regex as Regex.ByteString
    ~~~

### Template Haskell

GHC cannot run Template Haskell splices from an indefinite package because
indefinite code is type-checked but not compiled — there is no object code to
execute at splice time. Splicing TH code *from a definite package into* an
indefinite one works fine. This is a GHC limitation, not a Stack limitation.

## Backpack in Stack

### Private Backpack

When all signatures and their implementations live inside the same package (for
example using sublibraries), no special build orchestration is needed. This has
always worked in Stack without any extra configuration.

### Cross-package Backpack

When a signature is defined in one package and filled by a module from a
different package, Stack needs to perform an extra build step: after building
the indefinite package and the implementing package, it creates an
*instantiation task* that compiles the indefinite package against the concrete
implementation.

Stack handles this automatically. There is nothing you need to add to
`stack.yaml` beyond listing the packages as usual:

~~~yaml
packages:
- sig-pkg
- impl-pkg
- consumer-pkg
~~~

As long as the package description files set up the signatures, dependencies,
and mixins correctly, `stack build` does the rest.

### What happens during a build

When Stack encounters a cross-package Backpack setup, the build output shows the
extra instantiation step:

~~~text
sig-pkg   > configure (lib)
sig-pkg   > build (lib)
impl-pkg  > configure (lib)
impl-pkg  > build (lib)
sig-pkg   > build (inst:941095d7: Str = impl-pkg)
consumer  > configure (lib)
consumer  > build (lib)
~~~

The line marked `inst:` is the instantiation task. The hash identifies the
particular combination of signature-to-implementation mappings. The output also
shows which signatures are filled and by which packages.

### Supported features

Stack supports the full set of Backpack features that Cabal (the library)
exposes:

* Signature modules and indefinite packages
* Mixin linking (filling signatures by bringing a same-named module into scope)
* Explicit renaming in `mixins` (`requires (Sig as Impl)`)
* Multiple instantiations of the same indefinite package with different
  implementations
* Sub-library signatures and implementations
* Transitive Backpack chains (an indefinite package depending on another
  indefinite package — all inherited signatures are filled)
* Indefinite packages from Hackage or Stackage snapshots (not just local
  packages)
* Haddock generation for instantiated packages
* Precompiled caching of instantiation results

### Limitations

**`requires hiding` with partial instantiation.** If a mixin uses
`requires hiding (SomeSig)` to leave a signature unfilled, Stack will not create
an instantiation task for that mixin. Cabal (the library) requires all
signatures to be filled in a single instantiation — partial instantiation is not
possible. This means the indefinite package remains indefinite for the hidden
signatures and a higher-level consumer must fill them. When `hiding` hides
nothing (i.e. `requires hiding ()`) it is equivalent to `DefaultRenaming` and
works normally.

**Template Haskell in indefinite packages.** As described above, this is a GHC
restriction, not specific to Stack.

## Further reading

* [Using GHC - Filenames and separate compilation - Module signatures](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#module-signatures)
  in the GHC User's Guide
* [How to use Backpack modules](https://cabal.readthedocs.io/en/latest/how-to-use-backpack.html)
  in the Cabal project's documentation
* [Backpack: Retrofitting Haskell with Interfaces](https://plv.mpi-sws.org/backpack/)
  — the original paper
* [Try Backpack: Cabal packages](http://blog.ezyang.com/2017/01/try-backpack-cabal-packages/)
  — a practical walkthrough by Edward Z. Yang
* [GHC wiki: Backpack](https://gitlab.haskell.org/ghc/ghc/-/wikis/backpack)
