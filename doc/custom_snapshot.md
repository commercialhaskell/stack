# Custom Snapshots

Custom snapshots allow you to create your own snapshots, which provide a list of
specific hackage packages to use, along with flags and ghc-options.  The
definition of a basic snapshot looks like the following:

```yaml
resolver: ghc-8.0

packages:
  - unordered-containers-0.2.7.1
  - hashable-1.2.4.0
  - text-1.2.2.1

flags:
  unordered-containers:
    debug: true
```

If you put this in a `snapshot.yaml` file in the same directory as your project,
you can now use the custom snapshot like this:

```yaml
resolver:
  name: simple-snapshot  # Human readable name for the snapshot
  location: simple-snapshot.yaml
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

## Extending snapshots

The example custom snapshot above uses a compiler resolver, and so has few
packages.  We can also extend existing snapshots, by using the usual
[resolver setting found in stack configurations](yaml_configuration.md#resolver).
All possible resolver choices are valid, so this means that custom snapshots can
even extend other custom snapshots.

Lets say that we want to use `lts-7.1`, but use a different version of `text`
than the one it comes with, `1.2.2.1`.  To downgrade it to `1.2.2.0`, we need a
custom snapshot file with the following:

```yaml
resolver: lts-7.1
packages:
  - text-1.2.2.0
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
ghc-options:
  text:
    developer: true
```

## YAML format

In summary, the YAML format of custom snapshots has the following fields which
are directly related to the same fields in the
[build configuration format](yaml_configuration.md):

* `resolver`, which specifies which snapshot to extend. It takes the same values
  as the [`resolver` field in stack.yaml](yaml_configuration.md#resolver).

* `compiler`, which specifies or overrides the selection of compiler. If
  `resolver` is absent, then a specification of `compiler` is required. Its
  semantics are the same as the
  [`compiler` field in stack.yaml](yaml_configuration.md#compiler).

Some fields look similar, but behave differently:

* `flags` specifies which cabal flags to use with each package. In order to
  specify a flag for a package, it *must* be listed in the `packages` list.

* `ghc-options`, which specifies which cabal flags to use with each package. In
  order to specify ghc-options for a package, it *must* be listed in the
  `packages` list. The `*` member of the map specifies flags that apply to every
  package in the `packages` list.

There are two fields which work differently than in the build configuration
format:

* `packages`, which specifies a list of hackage package versions.  Note that
  when a package version is overridden, no `flags` or `ghc-options` are taken
  from the snapshot that is being extended.  If you want the same options as the
  snapshot being extended, they must be re-specified.

* `drop-packages`, which specifies a list of packages to drop from the snapshot
  being overridden.

## Future enhancements

We plan to enhance extensible snapshots in several ways in the future. See
[issue #1265, about "implicit snapshots"](https://github.com/commercialhaskell/stack/issues/1265).
In summary, in the future:

1) It will be possible to use a specific git repository + commit hash in the
`packages` list, like in regular stack.yaml configuration. Currently, custom
snapshots only work with packages on hackage.

2) `stack.yaml` configurations will implicitly create a snapshot. This means
that the non-local packages will get shared between your projects, so there is
less redundant compilation!

3) `flags` and `ghc-options` for packages which are not listed in `packages` are
silently ignored. See
[#2654](https://github.com/commercialhaskell/stack/issues/2654) for the current
status of this.
