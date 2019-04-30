<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Lock Files

Stack attempts to provide reproducible build plans. This involves
reproducibly getting the exact same contents of source packages and
configuration options (like cabal flags and GHC options) for a given
set of input files. There are a few problems with making this work:

* Entering all of the information to fully provide reproducibility is
  tedious. This would include things like Hackage revisions, hashes of
  remote tarballs, etc. Users don't want to enter this information.
* Many operations in Stack rely upon a "snapshot hash," which
  transitively includes the completed information for all of these
  dependencies. If any of that information is missing when parsing the
  `stack.yaml` file or snapshot files, it could be expensive for Stack
  to calculate it.

To address this, we follow the (fairly standard) approach of having a
_lock file_. The goal of the lock file is to cache completed
locations of project, snapshot packages and snapshots themselves so that:

* These files can be stored in source control
* Users on other machines can reuse these lock files and get identical
  build plans given that the used local packages and local snapshots are
  the same on those machines
* Rerunning `stack build` in the future is deterministic in the build
  plan, not depending on mutable state in the world like Hackage
  revisions
    * **NOTE** If, for example, a tarball available remotely is
      deleted or the hash changes, it will not be possible for Stack
      to perform the build. However, by deterministic, we mean it
      either performs the same build or fails, never accidentally
      doing something different.

This document explains the contents of a lock file, how they are used,
and how they are created and updated.

## stack.yaml and snapshot files

Relevant to this discussion, the `stack.yaml` file specifies:

* Resolver (the parent snapshot)
* `extra-deps`

The resolver can either specify a compiler version or another snapshot
file. This snapshot file can contain the same information referenced
above for a `stack.yaml`, with the following differences:

* The `extra-deps` are called `packages`
* Drop packages can be included

Some information in these files can be incomplete. Consider:

```yaml
resolver: lts-13.9
packages: []
extra-deps:
- https://hackage.haskell.org/package/acme-missiles-0.3.tar.gz
```

This information is _incomplete_, since the contents of that URL may
change in the future. Instead, you could specify enough information in
the `stack.yaml` file to fully resolve that package. That looks like:

```yaml
extra-deps:
- size: 1442
  url: https://hackage.haskell.org/package/acme-missiles-0.3.tar.gz
  cabal-file:
    size: 613
    sha256: 2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1
  name: acme-missiles
  version: '0.3'
  sha256: e563d8b524017a06b32768c4db8eff1f822f3fb22a90320b7e414402647b735b
  pantry-tree:
    size: 226
    sha256: 614bc0cca76937507ea0a5ccc17a504c997ce458d7f2f9e43b15a10c8eaeb033
```

Users don't particularly feel like writing all of that. Therefore,
it's common to see _incomplete_ information in a `stack.yaml` file.

Additionally, the `lts-13.9` information is _also_ incomplete. While
we assume in general that LTS snapshots never change, there's nothing
that technically prohibits that from happening. Instead, the complete
version of that field is:

```yaml
resolver:
  size: 496662
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/13/9.yaml
  sha256: 83de9017d911cf7795f19353dba4d04bd24cd40622b7567ff61fc3f7223aa3ea
```

Also something people don't feel like writing by hand.

## Recursive snapshot layers

Snapshot files can be _recursive_, where `stack.yaml` refers to
`foo.yaml`, which refers to `bar.yaml`, which refers to `baz.yaml`. A
local snapshot file can refer to a remote snapshot file (available via
an HTTP(S) URL).

We need to encode information from _all_ of these snapshot layers and
the `stack.yaml` file in the lock file, to ensure that we can detect
if anything changes.

## Performance

In addition to acting as a pure correctness mechanism, the design of a
lock file given here also works as a performance improvement. Instead
of requiring that all snapshot files be fully parsed on each Stack
invocation, we can store information in the lock file and bypass
parsing of the additional files in the common case of no changes.

## Lock file contents

The lock file contains the following information:

* Completed package locations for both `extra-deps` and packages in
  snapshot files
    * **NOTE** This only applies to _immutable_ packages. Mutable
      packages are not included in the lock file.
* Completed information for the snapshot locations

It looks like the following:

```yaml
# Lock file, some message about the file being auto-generated
snapshots:
  # Starts with the snapshot specified in stack.yaml,
  # then continues with the snapshot specified in each
  # subsequent snapshot file
  - original:
      foo.yaml # raw content specified in a snapshot file
    completed:
      file: foo.yaml
      sha256: XXXX
      size: XXXX
  - original:
      lts-13.9
    completed:
      size: 496662
      url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/13/9.yaml
      sha256: 83de9017d911cf7795f19353dba4d04bd24cd40622b7567ff61fc3f7223aa3ea

packages:
- original: https://hackage.haskell.org/package/acme-missiles-0.3.tar.gz
  completed:
    size: 1442
    url: https://hackage.haskell.org/package/acme-missiles-0.3.tar.gz
    cabal-file:
      size: 613
      sha256: 2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1
    name: acme-missiles
    version: '0.3'
    sha256: e563d8b524017a06b32768c4db8eff1f822f3fb22a90320b7e414402647b735b
    pantry-tree:
      size: 226
      sha256: 614bc0cca76937507ea0a5ccc17a504c997ce458d7f2f9e43b15a10c8eaeb033
```

## Creation

Whenever a `stack.yaml` file is loaded, Stack checks for a lock file
in the same file path, with a `.lock` extension added. For example, if
you run `stack build --stack-yaml stack-11.yaml`, it will use a lock
file in the location `stack-11.yaml.lock`. For the rest of this
document, we'll assume that the files are simply `stack.yaml` and
`stack.yaml.lock`.

If the lock file does not exist, it will be created by:

* Loading the `stack.yaml`
* Loading all snapshot files
* Completing all missing information
* Writing out the new `stack.yaml.lock` file

## Update procedure

When loading a Stack project all completed package or snapshot locations
(even when they were completed using information from a lock file) get
collected to form a new lock file in memory and compare against the one
on disk, writing if there are any differences.
