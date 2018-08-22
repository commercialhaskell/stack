<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Pantry in Stack

Beginning with Stack 1.11, Stack uses the Pantry library for its
specification of snapshots and package locations. Under the surface,
Pantry is geared towards reproducible build plans with
cryptographically secure specification of packages and snapshots.

There are three user-visible components to Pantry's configuration which affect usage of Stack:

* Snapshot location specification (in the `resolver` field)
* Package location specification (in the `extra-deps` field and inside snapshots)
* Snapshot specification, for creating custom snapshots

## Freeze command

As you'll see throughout this document, there is a lot of additional
information that can be provided to Stack to make the configuration
more reproducible and faster to parse. However, it's tedious to
specify these values manually. Therefore, the recommended workflow is:

* Manually write the simple version of a configuration value
* Use `stack freeze` to obtain the more reproducible version

See [freeze command details](#freeze-command-details) for more
information.

## Snapshot location

There are essentially four different ways of specifying a snapshot
location:

* Via a compiler version, which is a "compiler only" snapshot. This
  could be, e.g., `resolver: ghc-8.4.3`.
* Via a URL pointing to a snapshot configuration file, e.g. `resolver: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/nightly/2018/8/21.yaml`
* Via a local file path pointing to a snapshot configuration file, e.g. `resolver: my-local-snapshot.yaml`
* Via a _convenience synonym_, which provides a short form for some
  common URLs. These are:
    * Github: `github:user/repo:path` is treated as `https://raw.githubusercontent.com/user/repo/master/path`
    * LTS Haskell: `lts-X.Y` is treated as `github:commercialhaskell/stackage-snapshots:lts/X/Y.yaml`
    * Stackage Nightly: `nightly-YYYY-MM-DD` is treated as `github:commercialhaskell/stackage-snapshots:nightly/YYYY/M/D.yaml`

For safer, more reproducible builds, you can optionally specify a URL
together with a cryptographic hash of its content, e.g.:

```yaml
resolver:
  size: 499143
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/12/0.yaml
  sha256: 781ea577595dff08b9c8794761ba1321020e3e1ec3297fb833fe951cce1bee11
```

Where the `size` is the number of bytes in the file, and `sha256` is
its SHA256 hash. This information can automatically be generated with
the [`stack freeze`](#freeze-command) command.

## Package location

Pantry supports three types of package locations:

* Hackage packages
* Repositories
* Archives

All three of these formats support optional tree metadata to be added,
which can be used for reproducibility and faster downloads. This
information can automatically be generated with the [`stack
freeze`](#freeze-command) command.

### Hackage

Packages can be stated by a name/version combination. The basic syntax
for this is:

```yaml
extra-deps:
- acme-missiles-0.3
```

Using this syntax, the most recent Cabal file revision available will
be used. For more reproducibility of builds, it is recommended to
state the SHA256 hash of the cabal file contents as well, like this:

```yaml
extra-deps:
- acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1
```

Or, better yet, including the cabal file size too:

```yaml
extra-deps:
- acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1,631
```

Or a specific revision number, with `0` being the original file:

```yaml
extra-deps:
- acme-missiles-0.3@rev:0
```

Note that specifying via SHA256 is slightly more resilient in that it
does not rely on correct ordering in the package index, while revision
number is likely simpler to use. In practice, both should guarantee
equally reproducible build plans.

Finally, you can include the Pantry tree information. The following
was generated with `stack freeze`:

```yaml
- hackage: acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1,613
  pantry-tree:
    size: 226
    sha256: 614bc0cca76937507ea0a5ccc17a504c997ce458d7f2f9e43b15a10c8eaeb033
```

### Git and Mercurial repos

You can give a Git or Mercurial repo at a specific commit, and Stack
will clone that repo.

```yaml
extra-deps:
- git: git@github.com:commercialhaskell/stack.git
  commit: 6a86ee32e5b869a877151f74064572225e1a0398
- git: git@github.com:snoyberg/http-client.git
  commit: "a5f4f3"
- hg: https://example.com/hg/repo
  commit: da39a3ee5e6b4b0d3255bfef95601890afd80709
```

__NOTE__ It is highly recommended that you only use SHA1 values for a
Git or Mercurial commit. Other values may work, but they are not
officially supported, and may result in unexpected behavior (namely,
Stack will not automatically pull to update to new versions).
Another problem with this is that your build will not be deterministic,
because when someone else tries to build the project they can get a
different checkout of the package.

A common practice in the Haskell world is to use "megarepos", or
repositories with multiple packages in various subdirectories. Some
common examples include [wai](https://github.com/yesodweb/wai/) and
[digestive-functors](https://github.com/jaspervdj/digestive-functors). To
support this, you may also specify `subdirs` for repositories, e.g.:

```yaml
extra-deps:
- git: git@github.com:yesodweb/wai
  commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
  subdirs:
  - auto-update
  - wai
```

Since v1.7.1, you can specify packages from GitHub repository name using `github`:

```yaml
extra-deps:
- github: snoyberg/http-client
  commit: a5f4f30f01366738f913968163d856366d7e0342
```

If unspecified, `subdirs` defaults to `['.']` meaning looking for a
package in the root of the repo.  Note that if you specify a value of
`subdirs`, then `'.'` is _not_ included by default and needs to be
explicitly specified if a required package is found in the top-level
directory of the repository.

Using the `stack freeze` command will add in additional information,
including not only the Pantry tree hash, but also package metadata
which can allow Stack to work faster by bypassing cabal file
parses. For example, this:

```yaml
extra-deps:
- git: git@github.com:yesodweb/wai
  commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
  subdirs:
  - auto-update
  - wai
```

Would be converted into:

```yaml
extra-deps:
- subdir: auto-update
  cabal-file:
    size: 1219
    sha256: c07b2b1a2df1199f83eef819ac9bb067567e100b60586a52f8b92fc733ae3a6d
  name: auto-update
  version: 0.1.2.1
  git: git@github.com:yesodweb/wai
  pantry-tree:
    size: 687
    sha256: 26377897f35ccd3890b4405d72523233717afb04d62f2d36031bf6b18dcef74f
  commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
- subdir: wai
  cabal-file:
    size: 1717
    sha256: 7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056
  name: wai
  version: 3.0.2.3
  git: git@github.com:yesodweb/wai
  pantry-tree:
    size: 10299
    sha256: ce33fddab13592c847fbd7acd1859dfcbb9aeb6c212db3cee27c909fa3f3ae44
  commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
```

### Archives (HTTP(S) or local filepath)

You can use HTTP and HTTPS URLs and local filepaths referring to
either tarballs or ZIP files.

__NOTE__ Stack assumes that these files never change after downloading
to avoid needing to make an HTTP request on each build. Use hashes to
provide more security.

```yaml
extra-deps:
- https://example.com/foo/bar/baz-0.0.2.tar.gz
- archive: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
  subdirs:
  - wai
  - warp
- archive: ../acme-missiles-0.3.tar.gz
  sha256: e563d8b524017a06b32768c4db8eff1f822f3fb22a90320b7e414402647b735b
```

With the `stack freeze` command, this would be replaced with:

```yaml
extra-deps:
- size: 1540
  url: https://hackage.haskell.org/package/acme-dont-1.1.tar.gz
  cabal-file:
    size: 602
    sha256: 8264ad3e5113d3e0417b46e71d5a9c0914a1f03b5b81319cc329f1dc0f49b96c
  name: acme-dont
  version: '1.1'
  sha256: c32231ff8548bccd4f3bafcc9b1eb84947a2e5e0897c50c048e0e7609fc443ce
  pantry-tree:
    size: 206
    sha256: 79dbeddaf0fd507611687cefe9511c8fda489849fb0cac3894925716936290b2
- size: 285152
  subdir: wai
  url: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
  cabal-file:
    size: 1717
    sha256: 7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056
  name: wai
  version: 3.0.2.3
  sha256: 3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba
  pantry-tree:
    size: 10296
    sha256: ce431f1a22fcda89375ba5e35e53aee968eea23d1124fcba7cb9eae426daa2db
- size: 285152
  subdir: warp
  url: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
  cabal-file:
    size: 6648
    sha256: e3f01fd7417af923fd30962e9e6a4fe4de41ebc5e02af9819067fed79c9c6575
  name: warp
  version: 3.0.13.1
  sha256: 3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba
  pantry-tree:
    size: 4292
    sha256: d6b1def306a042b5fc500930302533a3ea828e916c99cbd82c0b7e2c4e3a8e09
- size: 1442
  filepath: acme-missiles-0.3.tar.gz
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

## Snapshots

_NOTE_ Stack has supported custom snapshots properly since version
1.6. In version 1.11, the support for snapshots was moved to Pantry,
and Stackage snapshots have moved over to using the same
format. Therefore, there is no longer such a thing as "custom
snapshots," there are simply "snapshots." Pantry snapshots follow the
same format as Stack 1.6 "custom snapshots."

Snapshots provide a list of packages to use, along with flags,
ghc-options, and a few other settings. Snapshots may extend any other
snapshot that can be specified in a `resolver` field. The packages
specified follow the same syntax mentioned above for
dependencies. Unlike `extra-deps`, however, no support for local
directories is available in snapshots to ensure reproducibility.

```yaml
resolver: lts-8.21 # Inherits GHC version and package set
compiler: ghc-8.0.1 # Overwrites GHC version in the resolver, optional

name: my-snapshot # User-friendly name

# Additional packages, follows extra-deps syntax
packages:
- unordered-containers-0.2.7.1
- hashable-1.2.4.0
- text-1.2.2.1

# Override flags, can also override flags in the parent snapshot
flags:
  unordered-containers:
    debug: true

# Packages from the parent snapshot to ignore
drop-packages:
- wai-extra

# Packages which should be hidden (affects script command's import
# parser
hidden:
  wai: true
  warp: false

# Set GHC options for specific packages
ghc-options:
  warp:
  - -O2
```

If you put this in a `snapshot.yaml` file in the same directory as your project,
you can now use the custom snapshot like this:

```yaml
resolver: snapshot.yaml
```

This is an example of a custom snapshot stored in the filesystem. They are
assumed to be mutable, so you are free to modify it. We detect that the snapshot
has changed by hashing the contents of the involved files, and using it to
identify the snapshot internally. It is often reasonably efficient to modify a
custom snapshot, due to stack sharing snapshot packages whenever possible.

Running the `stack freeze --snapshot` command yields the following
output:

```yaml
flags:
  unordered-containers:
    debug: true
ghc-options:
  warp:
  - -O2
packages:
- hackage: unordered-containers-0.2.7.1@sha256:7a1ceb6d88c0f16ec417f28dac16f6dc7b10e88fbb536a74d84941ad2f57b74b,4367
  pantry-tree:
    size: 1286
    sha256: 8a8f745cacae3c11a9c6e6c2fcefc95a13d0c153a8e14b4d28485db1b59d9ef3
- hackage: hashable-1.2.4.0@sha256:33a49b3ea87cc4a0c89a4fd48f19e4807d8c620aff710a048a28cf7d9c9b4620,4271
  pantry-tree:
    size: 1325
    sha256: cb05c31a8ec43f727004e5a6c8e35ff92e0515855a85cb01fa73623683ee4b33
- hackage: text-1.2.2.1@sha256:1c6ffad395d1674915cc9fda1d3b8f202ddcbfda7c341eb8bd99de67d3283bf9,5724
  pantry-tree:
    size: 7376
    sha256: ac2601c49cf7bc0f5d66b2793eddc8352f51a6ee989980827a0d0d8169700a03
name: my-snapshot
hidden:
  warp: false
  wai: true
drop-packages:
- wai-extra
compiler: ghc-8.0.1
resolver:
  size: 515969
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/8/21.yaml
  sha256: 2ec73d520d3e55cb753eaca11a72a9ce95bd9ba7ccaf16de1150d0130a50a5a1
```

### Overriding the compiler

The following snapshot specification will be identical to `lts-7.1`, but instead
use `ghc-7.10.3` instead of `ghc-8.0.1`:

```yaml
resolver: lts-7.1
compiler: ghc-7.10.3
```

### Dropping packages

The following snapshot specification will be identical to `lts-7.1`, but without
the `text` package in our snapshot. Removing this package will cause all the
packages that depend on `text` to be unbuildable, but they will still be present
in the snapshot.

```yaml
resolver: lts-7.1
drop-packages:
  - text
```

### Specifying ghc-options

In order to specify ghc-options for a package, you use the same syntax as the
[ghc-options](yaml_configuration.md#ghc-options) field for build configuration.
The following snapshot specification will be identical to `lts-7.1`, but
provides `-O1` as a ghc-option for `text`:

```yaml
resolver: lts-7.1
packages:
  - text-1.2.2.1
ghc-options:
  text: -O1
```

This works somewhat differently than the stack.yaml `ghc-options` field, in that
options can only be specified for packages that are mentioned in the custom
snapshot's `packages` list. It sets the ghc-options, rather than extending those
specified in the snapshot being extended.

Another difference is that the `*` entry for `ghc-options` applies to all
packages in the `packages` list, rather than all packages in the snapshot.

### Specifying flags

In order to specify flags for a package, you use the same syntax as the
[flags](yaml_configuration.md#flags) field for build configuration. The
following snapshot specification will be identical to `lts-7.1`, but
it enables the `developer` cabal flag:

```yaml
resolver: lts-7.1
packages:
  - text-1.2.2.1
flags:
  text:
    developer: true
```

## Freeze command details

To make builds reproducible it makes sense to pin project dependencies to some
exact versions and this is what is stack's `freeze` command is about.

### Project freezing

The default mode of its invocation:

```
$ stack freeze
```
freezes the following fields from the project's `stack.yaml`

* packages in `extra-deps` which do not include sha256 of their cabal files and
  which do not specify pantry tree pointer of the package archive
* `resolver` if it references a remote snapshot and if it does not specify
  pantry tree pointer of its contents

The command outputs to standard output new project's `stack.yaml` with these
changes included.

If a project is specified precisely enough stack tells about it and exits.

### Snapshot freezing

When a project uses some custom snapshot freezing dependencies defined in
the project is not enough as a snapshot could also contain not precisely
specified package references. To prevent this from happening `--snapshot` flag
(or `-s` in its short form) of the `freeze` command could be used:

```
$ stack freeze --snapshot
```

In this mode `freeze` command works almost like in the default mode, the main
differenc is that it works with the projects snapshot definition and thus it
pins packages from its `packages` field and not from the project's `extra-deps`.
