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

By default:

*   a file is generated for each project package. In the alternative, one or
    more project package directories can be specified;

*   the command will check the package for common mistakes. Pass the flag
    `--ignore-check` to disable the checks;

*   Stack will not test the generated file by attempting to build it. Pass the
    flag `--test-tarball` to cause Stack to test the generated file;

*   the generated file will be created in the `dist` directory of the project
    package directory. For information about the directory's location, command
    [`stack path --dist-dir`](path_command.md). Pass the option
    ``--tar-dir <path_to_directory>` to also copy the file to the specified
    directory; and

*   no PVP version bounds are added to the Cabal file of the package. Pass the
    option `--pvp-bounds <pvp_bounds_mode>` to determine whether and, if so,
    how bounds should be added. The available modes for basic use are: `none`,
    `lower`, `upper`, and `both`. The available modes for use with Cabal file
    revisions are `lower-revision`, `upper-revision` and `both-revision`.

    For futher information, see the
    [`pvp-bounds`](../configure/yaml/non-project.md#pvp-bounds) non-project
    specific configuration option documentation.

!!! info

    The Cabal specification says that a Cabal package must include a `Setup.hs`
    or `Setup.lhs` file. In practice, Stack and Cabal (the tool) do not need
    such a file to be present to build if the package's build type is `Simple`,
    and Hackage will accept such a Cabal package missing such a file. If such a
    file is missing from such a package, Stack and Cabal (the tool) will not add
    one to the tarball. However the `sdist` action of Cabal (the library) will
    add one.

!!! note

    Stack checks a package for common mistakes using checks provided by Cabal
    (the library). The version of Cabal used by Stack to check may be
    older than the version used by Stack to build.
