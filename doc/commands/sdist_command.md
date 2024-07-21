<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack sdist` command

~~~text
stack sdist [DIR] [--pvp-bounds PVP-BOUNDS] [--ignore-check]
            [--[no-]test-tarball] [--tar-dir ARG]
~~~

Hackage only accepts packages for uploading in a standard form, a compressed
archive ('tarball') in the format produced by Cabal's `sdist` action.

`stack sdist` generates a file for your package, in the format accepted by
Hackage for uploads. The command will report the location of the generated file.

## `--ignore-check` flag

Pass the flag to disable checks of the package for common mistakes. By default,
the command will check the package for common mistakes.

## `--pvp-bounds` option

The `--pvp-bounds <pvp_bounds_mode>` option determines whether and, if so, how
PVP version bounds should be added to the Cabal file of the package. The
available modes for basic use are: `none`, `lower`, `upper`, and `both`. The
available modes for use with Cabal file revisions are `lower-revision`,
`upper-revision` and `both-revision`.

For futher information, see the
[YAML configuration](../configure/yaml/non-project.md#pvp-bounds) documentation.

## `--tar-dir` option

The `--tar-dir <path_to_directory>` option determines whether the package
archive should be copied to the specified directory.

## `--[no-]test-tarball` flag

Default: Disabled

Set the flag to cause Stack to test the resulting package archive, by attempting
to build it.
