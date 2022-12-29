<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack unpack` command

~~~text
stack unpack PACKAGE [--to ARG]
~~~

`stack unpack` downloads a tarball for the specified package and unpacks it.

By default:

*   the download is for the most recent version of the package in the package
    index (eg Hackage). Specify the package name and its version (for example,
    `acme-missiles-0.1.0.0`) for a particular version of the package; and

*   the package is unpacked into a directory named after the package and its
    version. Pass the option `--to <directory>` to specify the destination
    directory.
