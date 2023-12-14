<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack unpack` command

~~~text
stack unpack PACKAGE [--candidate] [--to DIR]
~~~

`stack unpack` downloads an archive file for one or more specified packages from
the package index (e.g. Hackage), or one or more specified package candidates,
and unpacks each archive.

By default:

*   the download is from the package index. Pass the flag `--candidate` to
    specify package candidates;

    !!! note

        Stack assumes that a package candidate archive is a `.tar.gz` file named
        after the package version and located at endpoint
        `package\<package_version>\candidate\`.

*   in the case of packages from the package index, the download is for the most
    recent version. Specify the package name and its version (for example,
    `acme-missiles-0.1.0.0`) for a particular version of a package or for a
    package candidate. In the case of packages from the package index,
    optionally, a specific revision in the package index can be specified
    by appending `@rev:<number>` or `@sha256:<sha>`; and

*   the package is unpacked into a directory named after the package and its
    version. Pass the option `--to <directory>` to specify the destination
    directory. The directory can be an absolute one or relative to the current
    directory.
