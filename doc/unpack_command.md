<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack unpack` command

~~~text
stack unpack PACKAGE [--to DIR]
~~~

`stack unpack` downloads an archive file for one or more specified packages from
the package index (e.g. Hackage) and unpacks each archive.

By default:

*   the download is for the most recent version of the package in the package
    index. Specify the package name and its version (for example,
    `acme-missiles-0.1.0.0`) for a particular version of the package.
    Optionally, a specific revision in the package index can be specified by
    appending `@rev:<number>` or `@sha256:<sha>`; and

*   the package is unpacked into a directory named after the package and its
    version. Pass the option `--to <directory>` to specify the destination
    directory. The directory can be an absolute one or relative to the current
    directory.
