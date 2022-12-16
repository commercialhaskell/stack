<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack init` command

~~~text
stack init [DIR(S)] [--omit-packages] [--force] [--ignore-subdirs]
~~~

`stack init` initialises Stack's project-level YAML configuration file
(`stack.yaml`) for an existing project, based on the Cabal file or
`package.yaml` file for each of its packages.

Stack searches for Cabal and `package.yaml` files in the current directory,
unless one or more directories are specified as arguments.

Stack also searches for Cabal and `package.yaml` files in subdirectories, unless
the `--ignore-subdirs` flag is passed.

Stack will not overwrite an existing `stack.yaml` file, unless the `--force`
flag is passed.

Pass the `--ignore-subdirs` flag to cause Stack to ignore conflicting or
incompatible user packages while initialising.
