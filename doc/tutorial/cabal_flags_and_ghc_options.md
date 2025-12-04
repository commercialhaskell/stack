  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 10. Cabal flags and GHC options

There are two common ways to affect how a package will be built: with Cabal
flags and with GHC options.

## Cabal flags

A package description may specify one or more Cabal flags:

=== "Cabal file"

    ~~~text
    flag my-flag-name
      description: My (optional) description of my flag.
      default: false -- Optional: the default value is true
      manual: true -- Optional: the default value is false
    ~~~

=== "`package.yaml` (Hpack)"

    ~~~yaml
    flags:
      my-flag-name:
        description: My (optional) description of my flag.
        default: true # Required
        manual: false # Required
    ~~~

??? question "How does `manual: false` affect Stack's builds?"

    `manual: false` has different implications for Cabal and Stack. Cabal tries
    to 'solve' dependencies using the flag’s default value and, if it can't,
    tries again with the negated default value. Stack emphasises reproducible
    builds. It only tries to build with the flag's default value and, if it
    can't, reports that it can't.

Cabal flags can be set or unset at the command line or as a project-specific
Stack option.

To set or unset a Cabal flag at the command line, we can use the `--flag`
option. The `yackage` package has an `upload` flag that is enabled by default.
We can command:

~~~text
stack build --flag yackage:-upload
~~~

This means: when compiling the `yackage` package, turn off the `upload` Cabal
flag (thus the `-` in `-upload`). Unlike other tools, Stack is explicit about
which package's flag you want to change. It does this for two reasons:

1. There is no global meaning for Cabal flags, and therefore two packages can
   use the same flag name for completely different things.

2. By following this approach, we can avoid unnecessarily recompiling snapshot
   packages that happen to use a Cabal flag that we are using.

You can also change Cabal flag values on the command line for extra-dep and
snapshot packages. If you do this, that package will automatically be promoted
to an extra-dep, since the build plan is different than what the plan snapshot
definition would entail.

If you have Cabal flags that you will be setting regularly when building your
packages, you can add them to your Stack project-level configuration file
(`stack.yaml`). For more information, see the
[flags](../configure/yaml/project.md#flags) project-specific configuration
option documentation.

## GHC options

GHC options can be specified at the command line or as an non-project specific
Stack option.

At the command line, consider the command:

~~~text
stack build --ghc-options="-Wall -Werror"
~~~

or, equivalently:

~~~text
stack build --ghc-options=--pedantic
~~~

By default, this will set GHC's `-Wall` and `-Werror` options for all *project
packages*. This will not, however, affect other packages at all. This design
provides us with reproducible and fast builds.

??? question "Can GHC options for other packages be specified at the command line?"

    Yes, GHC options can be specified at the command line for all packages or
    only project packages that are targets. For further information, see the
    documentation for the
    [apply-ghc-options](../configure/yaml/non-project.md#apply-ghc-options)
    non-project specific configuration option.

??? question "What if GHC options specified at the command line apply only to targets?"

    By changing the default using the
    [apply-ghc-options](../configure/yaml/non-project.md#apply-ghc-options)
    configuration option, it is possble to specify that GHC options at the
    command line apply only to project packages that are *targets*. If this is
    done and you change your targets, the options will no longer apply to other
    project packages.

    Let us consider an example from the `wai` repository, which includes the
    `wai` and `warp` packages, the latter depending on the former. If we
    command:

    ~~~text
    stack build --ghc-options=-O0 wai
    ~~~

    Stack will build all of the dependencies of `wai` (inclduding `warp`) and then
    build `wai` with all GHC optimizations disabled.

    Now let us add `warp` as a target. If we command:

    ~~~text
    stack build --ghc-options=-O0 wai warp
    ~~~

    this builds the additional dependencies for `warp`, and then builds `warp`
    with GHC optimizations disabled. Importantly, Stack does not rebuild `wai`,
    since `wai`'s configuration has not been altered.

    Now the surprising case. If we command:

    ~~~text
    stack build --ghc-options=-O0 warp
    ~~~

    you may expect this to do nothing, as neither `wai` nor `warp` has changed.
    However, Stack will rebuild `wai` with GHC optimizations enabled again, and
    then rebuild `warp` (with optimizations disabled) against this newly-built
    `wai`. The reason is reproducible builds. If we had never built `wai` or
    `warp` before, trying to build `warp` would require building all of its
    dependencies, and it would do so with default GHC options (that is, GHC
    optimizations enabled). These dependencies would include `wai`. So when we
    command:

    ~~~text
    stack build --ghc-options=-O0 warp
    ~~~

    we want Stack's behavior to be unaffected by any previous build steps we
    took.

If you have GHC options that you will be applying regularly when building your
packages, you can add them to your Stack project-level configuration file
(`stack.yaml`) or (if applicable) to a
[global Stack configuration file](../configure/yaml/index.md#project-level-and-global-configuration-files).
For more information, see the
[ghc-options](../configure/yaml/non-project.md#ghc-options) non-project specific
configuration option documentation.

??? question "Can Stack be configured to specify GHC options for specific packages?"

    Yes, Stack can be configured to specify GHC options for specific packages,
    either globally or at the project level. For more information, see the
    [ghc-options](../configure/yaml/non-project.md#ghc-options) non-project
    specific configuration option documentation.
