  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 10. Cabal flags and GHC options

There are two common ways to alter how a package will install: with Cabal flags
and with GHC options.

## Cabal flag management

To change a Cabal flag setting, we can use the command line `--flag` option. The
`yackage` package has an `upload` flag that is enabled by default. We can
command:

~~~text
stack build --flag yackage:-upload
~~~

This means: when compiling the `yackage` package, turn off the `upload` flag
(thus the `-` in `-upload`). Unlike other tools, Stack is explicit about which
package's flag you want to change. It does this for two reasons:

1. There's no global meaning for Cabal flags, and therefore two packages can
   use the same flag name for completely different things.
2. By following this approach, we can avoid unnecessarily recompiling snapshot
   packages that happen to use a flag that we're using.

You can also change flag values on the command line for extra-dep and snapshot
packages. If you do this, that package will automatically be promoted to an
extra-dep, since the build plan is different than what the plan snapshot
definition would entail.

## GHC options

GHC options follow a similar logic as in managing Cabal flags, with a few
nuances to adjust for common use cases. Let's consider the command:

~~~text
stack build --ghc-options="-Wall -Werror"
~~~

This will set the `-Wall -Werror` options for all *local targets*. Note that
this will not affect extra-dep and snapshot packages at all. This design
provides us with reproducible and fast builds.

(By the way: the above GHC options have a special convenience flag:
`--pedantic`.)

There's one extra nuance about command line GHC options: Since they only apply
to local targets, if you change your local targets, they will no longer apply
to other packages. Let's play around with an example from the `wai` repository,
which includes the `wai` and `warp` packages, the latter depending on the
former. If we command again:

~~~text
stack build --ghc-options=-O0 wai
~~~

It will build all of the dependencies of `wai`, and then build `wai` with all
optimizations disabled. Now let's add in `warp` as well. Command:

~~~text
stack build --ghc-options=-O0 wai warp
~~~

This builds the additional dependencies for `warp`, and then builds `warp` with
optimizations disabled. Importantly: it does not rebuild `wai`, since `wai`'s
configuration has not been altered. Now the surprising case. Command:

~~~text
stack build --ghc-options=-O0 warp
wai-3.0.3.0-5a49351d03cba6cbaf906972d788e65d: unregistering (flags changed from ["--ghc-options","-O0"] to [])
warp-3.1.3-a91c7c3108f63376877cb3cd5dbe8a7a: unregistering (missing dependencies: wai)
wai-3.0.3.0: configure
~~~

You may expect this to be a no-op: neither `wai` nor `warp` has changed.
However, Stack will instead recompile `wai` with optimizations enabled again,
and then rebuild `warp` (with optimizations disabled) against this newly built
`wai`. The reason: reproducible builds. If we'd never built `wai` or `warp`
before, trying to build `warp` would necessitate building all of its
dependencies, and it would do so with default GHC options (optimizations
enabled). This dependency would include `wai`. So when we command:

~~~text
stack build --ghc-options=-O0 warp
~~~

We want its behavior to be unaffected by any previous build steps we took.
While this specific corner case does catch people by surprise, the overall goal
of reproducible builds is - in the Stack maintainers' views - worth the
confusion.

Final point: if you have GHC options that you'll be regularly passing to your
packages, you can add them to your `stack.yaml` file. For more information, see
the [ghc-options](../configure/yaml/non-project.md#ghc-options) non-project
specific configuration option documentation.

!!! note

    That's it, the heavy content of this guide is done! Everything from here on
    out is simple explanations of commands. Congratulations!
