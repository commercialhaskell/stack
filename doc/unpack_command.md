<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack unpack` command

~~~text
stack unpack TARGET [--candidate] [--to DIR]
~~~

`stack unpack` downloads an archive file for one or more specified target
packages from the package index (e.g. Hackage), or one or more specified target
package candidates, and unpacks each archive into a subdirectory named after the
package version.

In the case of packages from the package index, a target can be a package
name only. In that case, by default:

*   if Stack's `--snapshot` option is not specified, the download is for the
    most recent version of the package in the package index. Stack will first
    seek to update the index; and

*   if Stack's `--snapshot` option is specified, the download is for the version
    of the package included directly in the specified snapshot.

!!! note

    Stackage snapshots do not include directly most GHC boot packages (packages
    that come with GHC and are included in GHC's global package database) but
    some snapshots may include directly some boot packages. In particular, some
    snapshots include directly `Win32` (which is a boot package on Windows)
    while others do not.

Otherwise, a target should specify a package name and version (for example,
`acme-missiles-0.3`). In the case of package versions from the package index,
optionally, a revision in the package index can be specified by appending
`@rev:<number>` or `@sha256:<sha>` (for example, `acme-missiles-0.3@rev:0`).

By default:

*   the download is from the package index. Pass the flag `--candidate` to
    specify package candidates; and

    !!! note

        Stack assumes that a package candidate archive is a `.tar.gz` file named
        after the package version and located at endpoint
        `package\<package_version>\candidate\`. This is true of Hackage.

*   the target is unpacked into a subdirectory of the current directory. Pass
    the option `--to <directory>` to specify an alternative destination
    directory to the current directory. The destination directory can be an
    absolute one or relative to the current directory.
