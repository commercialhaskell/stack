<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack setup` command

~~~text
stack setup [GHC_VERSION] [--[no-]reinstall] [--ghc-bindist URL]
            [--ghcjs-boot-options GHCJS_BOOT] [--[no-]ghcjs-boot-clean]
~~~

`stack setup` attempts to install a version of GHC - by default, the version
required by the project and only if it is not already available to Stack. For
example:

~~~text
stack setup
stack will use a sandboxed GHC it installed
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
~~~

Alternatively, the version of GHC to be installed can be specified as an
argument. For example `stack setup 9.0.2`.

Set the `--reinstall` flag (disabled by default) to attempt to install the
version of GHC regardless of whether it is already available to Stack.

The `--ghc-bindist <url>` option can be used to specify the URL of the GHC to be
downloaded and installed. This option requires the use of the `--ghc-variant`
option specifying a custom GHC variant.

If Stack is configured not to install GHC (`install-ghc: false` or passing the
`--no-install-ghc` flag) then `stack setup` will warn that the flag and the
command are inconsistent and take no action.
