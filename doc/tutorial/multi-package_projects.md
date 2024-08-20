  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 9. Multi-package projects

Until now, everything we have done with Stack has used a single-package project.
However, Stack's power truly shines when you're working on multi-package
projects. All the functionality you'd expect to work just does: dependencies
between packages are detected and respected, dependencies of all packages are
just as one cohesive whole, and if anything fails to build, the build commands
exits appropriately.

Let us demonstrate this with the `wai-app-static` and `yackage` packages,
starting in the root directory for all our Haskell projects. Command:

~~~text
mkdir multi
cd multi
stack unpack wai-app-static yackage
~~~

The last command should report something like:

~~~text
...
Unpacked wai-app-static (from Hackage) to .../multi/wai-app-static-3.1.9/.
Unpacked yackage (from Hackage) to .../multi/yackage-0.8.1/.
~~~

Then command:

~~~text
stack init
~~~

The command should report something like:

~~~text
Looking for Cabal or package.yaml files to use to initialise Stack's
project-level configuration file.

Using the Cabal packages:
* wai-app-static-3.1.9/
* yackage-0.8.1/

Cabal file warning in .../multi/yackage-0.8.1/yackage.cabal@47:40: version
operators used. To use version operators the package needs to specify at least
'cabal-version: >= 1.8'.
Cabal file warning in .../multi/yackage-0.8.1/yackage.cabal@21:36: version
operators used. To use version operators the package needs to specify at least
'cabal-version: >= 1.8'.
Selecting the best among 12 snapshots...

Note: Matches ...

Selected the snapshot ...
Initialising Stack's project-level YAML configuration file using snapshot ...
Considered 2 user packages.
Writing configuration to stack.yaml.
Stack's project-level YAML configuration file has been initialised.
~~~

Then command:

~~~text
stack build --haddock --test
~~~

Stack should build and test the project packages.

If you look at the `stack.yaml` file, you'll see exactly what you'd expect:

~~~yaml
snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/31.yaml
packages:
- wai-app-static-3.1.9
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
