# Custom Snapshots

Custom snapshots were totally reworked with the extensible snapshots
overhaul in Stack 1.6.0, see
[the writeup](https://www.fpcomplete.com/blog/2017/07/stacks-new-extensible-snapshots)
and
[PR #3249](https://github.com/commercialhaskell/stack/pull/3249)). This
documentation covers the new syntax only.

Custom snapshots allow you to create your own snapshots, which provide
a list of packages to use, along with flags, ghc-options, and a few
other settings. Custom snapshots may extend any other snapshot that
can be specified in a `resolver` field. The packages specified follow
the syntax of `extra-deps` in the `stack.yaml` file, with one
exception: to ensure reproducibility of snapshots, local directories
are not allowed for custom snapshots (as they are expected to change
regularly).

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

## Using a URL instead of a filepath

For efficiency, URLs are treated differently. If I uploaded the snapshot to
`https://domain.org/snapshot-1.yaml`, it is expected to be immutable. If you
change that file, then you lose any reproducibility guarantees.

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
