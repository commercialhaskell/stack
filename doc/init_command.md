<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack init` command

~~~text
stack init [DIR(S)] [--omit-packages] [--force] [--ignore-subdirs]
~~~

`stack init` initialises Stack's project-level YAML configuration file
(`stack.yaml`) for an existing project, based on the Cabal file or
`package.yaml` file for each of its packages.

By default:

* Stack searches for Cabal and `package.yaml` files in the current directory.
  Specify one or more directories as arguments to cause Stack to search them;

* Stack also searches for Cabal and `package.yaml` files in subdirectories. Pass
  the flag `--ignore-subdirs` to ignore subdirectories;

* Stack will not overwrite an existing `stack.yaml` file. Pass the flag
  `--force` to allow overwriting; and

* Stack will not initialise if there are conflicting or incompatable user
  packages. Pass the flag `--omit-packages` to cause Stack to ignore such
  matters while initialising.
