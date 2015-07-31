This page is intended to fully document all configuration options available in the stack.yaml file. Note that, as we're still in beta, this page is likely to be both *incomplete* and sometimes *inaccurate*. If you see such cases, please update the page, and if you're not sure how, open an issue labeled "question".

The stack.yaml configuration options break down into project specific and non-project specific options. The latter can be specified in your global config (/etc/stack/config) and user config (~/.stack/stack.yaml), as well as in the project config. The former must be in the project config.

## Project config

### packages

This lists all local packages. In the simplest usage, it will be a list of directories, e.g.:

```yaml
packages:
- dir1
- dir2
- dir3
```

However, it supports two other location types: an HTTP URL referring to a tarball that can be downloaded, and information on a Git repo to clone, together with this SHA1 commit. For example:

```yaml
packages:
- some-directory
- https://example.com/foo/bar/baz-0.0.2.tar.gz
- location:
    git: git@github.com:commercialhaskell/stack
    commit: 6a86ee32e5b869a877151f74064572225e1a0398
```

Note: it is highly recommended that you only use SHA1 values for a Git commit. Other values may work, but they are not officially supported, and may result in unexpected behavior (namely, stack will not automatically pull to update to new versions).

stack further allows you to tweak your packages by specifying two additional
settings:

* A list of subdirectories to build (useful for mega-repos like
[wai](https://github.com/yesodweb/wai/) or
[digestive-functors](https://github.com/jaspervdj/digestive-functors))
* Whether a package should be treated as a dependency, in which case it will only be built if demanded by a non-dependency, and its test suites and benchmarks will not be run. This is useful for tweaking an upstream package.
    * The current name for this is `valid-wanted`, which when `false` means "treat as a dependency." That's a confusing name choice, and therefore we're switching it to be `extra-dep: true` to mean "treat as dependency." You'll get a deprecation warning once that new naming is release.

To tie this all together, here's an example of the different settings:

```yaml
packages:
- local-package
- location: vendor/binary
  valid-wanted: false
- location:
    git: git@github.com:yesodweb/wai
    commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
  subdirs:
  - auto-update
  - wai
```

### extra-deps

This is a list of package identifiers for additional packages from upstream to
be included. This is usually used to augment an LTS Haskell or Stackage Nightly
snapshot with a package that is not present or is at an older version than you
wish to use.

```yaml
extra-deps:
- acme-missiles-0.3
```

### resolver

Specifies how dependencies are resolved. There are currently three options:

* LTS Haskell snapshot, e.g. `resolver: lts-2.14`
* Stackage Nightly snapshot, e.g. `resolver: nightly-2015-06-16`
* No snapshot, just use packages shipped with GHC, e.g. `resolver: ghc-7.10`

It's important to point out that each resolver identifies a GHC major version,
and stack will require that that version of GHC is used for building your code.

### flags

Flags can be set for each package separately, e.g.

```yaml
flags:
  package-name:
    flag-name: true
```

Flags will only affect packages in your `packages` and `extra-deps` settings.
Packages that come from the snapshot global database or are not affected.

### image
The image settings are used for the creation of container images using `stack container image`, e.g.
```yaml
image:
  docker:
    base: "fpco/stack-build"
    add:
      static: /data/static
```
`base` is the docker image that will be used to built upon. The `add` lines allow you to add additional directories to your image. You can also specify `entrypoints`. Your executables are placed in `/usr/local/bin`.

## Non-project config

### docker

See [Docker configuration](Docker#configuration).

### connection-count

Integer indicating how many simultaneous downloads are allowed to happen

Default: 8

### hide-th-loading

Strip out the "Loading ..." lines from GHC build output, produced when using Template Haskell

Default: true

### latest-snapshot-url

URL providing a JSON with information on the latest LTS and Nightly snapshots, used for automatic project configuration.

Default: https://www.stackage.org/download/snapshots.json

### package-indices

```yaml
package-indices:
- name: Hackage
  download-prefix: https://s3.amazonaws.com/hackage.fpcomplete.com/package/

  # at least one of the following must be present
  git: https://github.com/commercialhaskell/all-cabal-hashes.git
  http: https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz

  # optional fields, both default to false
  gpg-verify: false
  require-hashes: false
```

One thing you should be aware of: if you change the contents of package-version
combination by setting a different package index, this *can* have an effect on
other projects by installing into your shared snapshot database.

### system-ghc

Enables or disables using the GHC available on the PATH. Useful to disable if
you want to force stack to use its own installed GHC (via `stack setup`), in
cases where your system GHC my be incomplete for some reason. Default is true.

```yaml
# Turn off system GHC
system-ghc: false
```

### install-ghc

Whether or not to automatically install GHC when necessary. Default is false,
which means stack will prompt you to run `stack setup` as needed.

### skip-ghc-check

Should we confirm that your system GHC version (on the PATH) matches what your project expects? Default is true.

### require-stack-version

Require a version of stack within the specified range
([cabal-style](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information))
to be used for this project. Example: `require-stack-version: "== 0.1.*"`

Default: "-any"

### arch/os

Set the architecture and operating system for GHC, build directories, etc. Values are those recognized by Cabal, e.g.:

    arch: i386, x86_64
    os: windows, linux

You likely only ever want to change the arch value. This can also be set via the command line.

### extra-include-dirs/extra-lib-dirs

A list of extra paths to be searched for header files and libraries, respectively. Paths should be absolute

```yaml
extra-include-dirs:
- /opt/foo/include
extra-lib-dirs:
- /opt/foo/lib
```