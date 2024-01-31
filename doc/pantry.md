<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Snapshot and package location

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

This document describes:

* the specification of a snapshot location (in the
  [`snapshot`](yaml_configuration.md#snapshot) or
  [`resolver`](yaml_configuration.md#resolver) key)
* the specification of a package location (in the `extra-deps` key and in a
  snapshot)

!!! info

    Stack uses the [Pantry](https://hackage.haskell.org/package/pantry) to
    specify the location of snapshots and packages. Pantry is geared towards
    reproducible build plans with cryptographically secure specification of
    snapshots and packages.

## Snapshot location

There are essentially four different ways of specifying a snapshot location:

1.  Via a compiler version, which is a "compiler only" snapshot. This could be,
    for example:

    ~~~yaml
    snapshot: ghc-8.6.5`
    ~~~

2.  Via a URL pointing to a snapshot configuration file, for example:

    ~~~yaml
    snapshot: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/nightly/2018/8/21.yaml`
    ~~~

3.  Via a local file path pointing to a snapshot configuration file, for
    example:

    ~~~yaml
    snapshot: my-local-snapshot.yaml
    ~~~

4.  Via a _convenience synonym_, which provides a short form for some common
    URLs. These are:

    * GitHub: `github:user/repo:path` is treated as:

        ~~~text
        https://raw.githubusercontent.com/user/repo/master/path
        ~~~

    * LTS Haskell: `lts-X.Y` is treated (by default) as:

        ~~~text
        github:commercialhaskell/stackage-snapshots:lts/X/Y.yaml
        ~~~

    * Stackage Nightly: `nightly-YYYY-MM-DD` is treated (by default) as:

        ~~~text
        github:commercialhaskell/stackage-snapshots:nightly/YYYY/M/D.yaml
        ~~~

!!! info

    By default, LTS Haskell and Stackage Nightly snapshot configurations are
    retrieved from the `stackage-snapshots` GitHub repository of user
    `commercialhaskell`. The
    [snapshot-location-base](yaml_configuration.md#snapshot-location-base)
    option allows a custom location to be set.

For safer, more reproducible builds, you can optionally specify a URL
together with a cryptographic hash of its content. For example:

~~~yaml
snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/12/0.yaml
  size: 499143
  sha256: 781ea577595dff08b9c8794761ba1321020e3e1ec3297fb833fe951cce1bee11
~~~

`size` is the number of bytes in the file and `sha256` is the file's SHA256
hash. If not provided, the information will automatically be generated and
stored in a [lock file](lock_files.md).

## Package location

There are three types of package locations:

1.  Hackage packages
2.  Git and Mecurial repositories
3.  Local or remote archives (such as GitHub archives)

All three types support optional tree metadata to be added, which can be used
for reproducibility and faster downloads. This information can automatically be
generated in a [lock file](lock_files.md).

### Hackage packages

A package can be identified by its name, version and Cabal file revision
number, with revision `0` being the original Cabal file. For example:

~~~yaml
extra-deps:
- acme-missiles-0.3@rev:0
~~~

A package name and version only can be stated. Using this syntax, the most
recent Cabal file revision available in the package index will be used. For
example:

~~~yaml
extra-deps:
- acme-missiles-0.3
~~~

This syntax is often used in practice, but may result in one build differing
from another, if a new or further Cabal file revision is added to the package
index between the builds.

As an alternative to specifying the Cabal file revision number, you can specify
the package name and version with the SHA256 hash of the contents of its Cabal
file. Doing so is slightly more resilient than using the Cabal file revision
number, as it does not rely on the correct ordering in the package index.
For example:

~~~yaml
extra-deps:
- acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1
~~~

Optionally, you can specify also the size of the Cabal file in bytes. For
example (where the file size is `631` bytes):

~~~yaml
extra-deps:
- acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1,631
~~~

Optionally, you can specify also the Pantry tree information. For example:

~~~yaml
- hackage: acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1,613
  pantry-tree:
    size: 226
    sha256: 614bc0cca76937507ea0a5ccc17a504c997ce458d7f2f9e43b15a10c8eaeb033
~~~

A Pantry tree is a list of CAS (content-addressable storage)
'SHA256 hash'-'size in bytes' keys for each of the files in a package.

The SHA256 hash of the contents of the Cabal file and its size in bytes is
provided in Stack's lock file. For further information, see the
[lock files](lock_files.md) documentation. The SHA256 hash and file size
alternative is also what Stack uses when it makes suggestions about missing
packages.

### Git and Mercurial repositories

You can specify a Git or Mercurial repository at a specific commit, and Stack
will clone that repository and, if it has submodules (Git), update the
repository's submodules. For example:

~~~yaml
extra-deps:
- git: git@github.com:commercialhaskell/stack.git
  commit: '6a86ee32e5b869a877151f74064572225e1a0398'
- git: git@github.com:snoyberg/http-client.git
  commit: 'a5f4f3'
- hg: https://example.com/hg/repo
  commit: 'da39a3ee5e6b4b0d3255bfef95601890afd80709'
~~~

!!! note

    It is highly recommended that you only use SHA1 values for a Git or
    Mercurial commit. Other values may work, but they are not officially
    supported, and may result in unexpected behavior (namely, Stack will not
    automatically pull to update to new versions). Another problem with this is
    that your build will not be deterministic, because when someone else tries
    to build the project they can get a different checkout of the package.

!!! note

    The `commit:` key expects a YAML string. A commit hash, or partial hash,
    comprised only of digits represents a YAML number, unless it is enclosed in
    quotation marks.

!!! warning

    For the contents of a Git repository, Stack cannot handle filepaths or
    symbolic link names that are longer than those supported by the `ustar`
    (Unix Standard TAR) archive format defined by
    [POSIX.1-1988](https://nvlpubs.nist.gov/nistpubs/Legacy/FIPS/fipspub151-1.pdf).

    Stack uses `git archive` to convert the content of a Git repository to a
    TAR archive, which it then seeks to consume. Git produces `pax` format
    archives which use 'extended' headers for matters that the `ustar` format
    cannot handle. Unfortunately, Stack cannot consume an extended header and
    will silently discard the item.

A common practice in the Haskell world is to use "megarepos", or repositories
with multiple packages in various subdirectories. Some common examples include
[wai](https://github.com/yesodweb/wai/) and
[digestive-functors](https://github.com/jaspervdj/digestive-functors). To
support this, you may also specify `subdirs` for repositories. For example:

~~~yaml
extra-deps:
- git: git@github.com:yesodweb/wai
  commit: '2f8a8e1b771829f4a8a77c0111352ce45a14c30f'
  subdirs:
  - auto-update
  - wai
~~~

If unspecified, `subdirs` defaults to `['.']` meaning looking for a package in
the root of the repository. If you specify a value of `subdirs`, then `'.'` is
_not_ included by default and needs to be explicitly specified if a required
package is found in the top-level directory of the repository.

#### git-annex

[git-annex](https://git-annex.branchable.com) is not supported. This is because
`git archive` does not handle symbolic links outside the work tree. It is still
possible to use repositories which use git-annex but do not require the annex
files for the package to be built.

To do so, ensure that any files or directories stored by git-annex are marked
[export-ignore](https://git-scm.com/docs/git-archive#Documentation/git-archive.txt-export-ignore)
in the `.gitattributes` file in the repository. For further information, see
issue [#4579](https://github.com/commercialhaskell/stack/issues/4579).

For example, if the directory `fonts/` is controlled by git-annex, use the
following line:

~~~gitattributes
fonts export-ignore
~~~

### Local or remote archives (such as GitHub archives)

#### Filepaths or URLs to archive files

You can use filepaths referring to local archive files or HTTP or HTTPS URLs
referring to remote archive files, either tarballs or ZIP files.

!!! note

    An example of a remote archive file is a Hackage package candidate, usually
    located at (for example)
    https://hackage.haskell.org/package/my-package-1.0.0/candidate/my-package-1.0.0.tar.gz.

!!! warning

    Stack assumes that these archive files never change after downloading to
    avoid needing to make an HTTP request on each build.

For safer, more reproducible builds, you can optionally specify a cryptographic
hash of the archive file.

For example:

~~~yaml
extra-deps:
- https://example.com/foo/bar/baz-0.0.2.tar.gz
- archive: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
  subdirs:
  - wai
  - warp
- archive: ../acme-missiles-0.3.tar.gz
  sha256: e563d8b524017a06b32768c4db8eff1f822f3fb22a90320b7e414402647b735b
~~~

#### GitHub archive files

[:octicons-tag-24: 1.7.1](https://github.com/commercialhaskell/stack/releases/tag/v1.7.1)

You can specify a GitHub respository at a specific commit and Stack will obtain
from GitHub an archive file of the files in the repository at that point in its
history. For example:

~~~yaml
extra-deps:
- github: snoyberg/http-client
  commit: 'a5f4f30f01366738f913968163d856366d7e0342'
~~~

!!! note

    An archive file of the files in a GitHub repository at a point in its
    history is not the same as a clone of the repository (including its history)
    and the updating of any submodules. If you need the latter, use the syntax
    for a [Git repository](pantry.md#git-and-mercurial-repositories).

    If the package fails to build due to missing files, it may be that updated
    submodules are required.
