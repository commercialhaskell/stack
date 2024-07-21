  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 7. Multi-package projects

Until now, everything we've done with Stack has used a single-package project.
However, Stack's power truly shines when you're working on multi-package
projects. All the functionality you'd expect to work just does: dependencies
between packages are detected and respected, dependencies of all packages are
just as one cohesive whole, and if anything fails to build, the build commands
exits appropriately.

Let's demonstrate this with the `wai-app-static` and `yackage` packages,
starting in the root directory for all our Haskell projects. Command:

~~~text
mkdir multi
cd multi
stack unpack wai-app-static yackage
Unpacked wai-app-static (from Hackage) to .../multi/wai-app-static-3.1.7.4/
Unpacked yackage (from Hackage) to .../multi/yackage-0.8.1/
stack init
Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- wai-app-static-3.1.7.4/
- yackage-0.8.1/

Cabal file warning in .../multi/yackage-0.8.1/yackage.cabal@47:40: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
Cabal file warning in .../multi/yackage-0.8.1/yackage.cabal@21:36: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
Selecting the best among 18 snapshots...

* Matches ...

Selected resolver: ...
Initialising configuration using resolver: ...
Total number of user packages considered: 2
Writing configuration to file: stack.yaml
stack build --haddock --test
# Goes off to build a whole bunch of packages
~~~

If you look at the `stack.yaml` file, you'll see exactly what you'd expect:

~~~yaml
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/17.yaml
packages:
- wai-app-static-3.1.7.4
- yackage-0.8.1
~~~

Notice that multiple directories are listed in the `packages` key.

In addition to local directories, you can also refer to packages available in a
Git repository or in a tarball over HTTP/HTTPS. This can be useful for using a
modified version of a dependency that hasn't yet been released upstream.

!!! note

    When adding upstream packages directly to your project it is important to
    distinguish _project packages_ located locally from the upstream
    _dependency packages_. Otherwise you may have trouble running `stack ghci`.
    See [stack.yaml documentation](../configure/yaml/project.md#packages) for
    more details.
