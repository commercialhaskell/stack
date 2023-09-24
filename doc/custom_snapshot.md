<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

## Snapshot specification

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Snapshots provide a list of packages to use, along with flags, GHC options, and
a few other settings. Snapshots may extend any other snapshot that can be
specified in a `resolver` or `snapshot` key. The packages specified follow the
same syntax for dependencies in Stack's project-level configuration files.
Unlike the `extra-deps` key, however, no support for local directories is
available in snapshots to ensure reproducibility.

!!! info

    Stack uses the [Pantry](https://hackage.haskell.org/package/pantry) for
    snapshot specification.

~~~yaml
resolver: lts-21.13 # Inherits GHC version and package set
compiler: ghc-9.6.2 # Overwrites GHC version in the resolver, optional

# Additional packages, follows extra-deps syntax
packages:
- unordered-containers-0.2.7.1
- hashable-1.2.4.0
- text-1.2.2.1

# Packages from the parent snapshot to ignore
drop-packages:
- wai-extra

# Packages which should be hidden
hidden:
  wai: true
  warp: false

# Set GHC options for specific packages
ghc-options:
  warp:
  - -O2

# Override flags, can also override flags in the parent snapshot
flags:
  unordered-containers:
    debug: true
~~~

If you put this in a `snapshot.yaml` file in the same directory as your project,
you can now use the snapshot like this:

~~~yaml
resolver: snapshot.yaml
~~~

This is an example of a custom snapshot stored in the filesystem. They are
assumed to be mutable, so you are free to modify it. We detect that the snapshot
has changed by hashing the contents of the involved files, and using it to
identify the snapshot internally. It is often reasonably efficient to modify a
custom snapshot, due to Stack sharing snapshot packages whenever possible.

### Overriding the compiler

The following snapshot specification will be identical to `lts-21.13`, but
instead use `ghc-9.4.5` instead of `ghc-9.4.7`:

~~~yaml
resolver: lts-21.13
compiler: ghc-9.4.5
~~~

### Dropping packages

The following snapshot specification will be identical to `lts-21.13`, but
without the `text` package in our snapshot. Removing this package will cause all
the packages that depend on `text` to be unbuildable, but they will still be
present in the snapshot.

~~~yaml
resolver: lts-21.13
drop-packages:
- text
~~~

### Hiding packages

The following snapshot specification will be identical to `lts-21.13`, but the
`text` package will be hidden when registering. This will affect, for example,
the import parser in the script command.

~~~yaml
resolver: lts-21.13
hidden:
- text
~~~

### Specifying GHC options

In order to specify GHC options for a package, you use the same syntax as the
[ghc-options](yaml_configuration.md#ghc-options) key for build configuration.

The following snapshot specification will be identical to `lts-21.13`, but
provides `-O1` as a ghc-option for `text`:

~~~yaml
resolver: lts-21.13
packages:
- text-2.0.2
ghc-options:
  text: -O1
~~~

This works somewhat differently than the stack.yaml `ghc-options` field, in that
options can only be specified for packages that are mentioned in the custom
snapshot's `packages` list. It sets the ghc-options, rather than extending those
specified in the snapshot being extended.

Another difference is that the `*` entry for `ghc-options` applies to all
packages in the `packages` list, rather than all packages in the snapshot.

### Specifying Cabal flags

In order to specify Cabal flags for a package, you use the same syntax as the
[flags](yaml_configuration.md#flags) key for build configuration. The
following snapshot specification will be identical to `lts-21.13`, but
it enables the `developer` Cabal flag:

~~~yaml
resolver: lts-21.13
packages:
- text-2.0.2
flags:
  text:
    developer: true
~~~
