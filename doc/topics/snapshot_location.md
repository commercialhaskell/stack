<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Snapshot location

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

This document describes the specification of a snapshot location:

* **in YAML configuration files**, in the
  [`snapshot`](../configure/yaml/project.md#snapshot) or
  [`resolver`](../configure/yaml/project.md#resolver) key; or
* **at the command line**, with the
  [`--snapshot`](../configure/global_flags.md#-snapshot-option) or
  [`--resolver`](../configure/global_flags.md#-resolver-option) option.

## In YAML configuration files

!!! info

    Stack uses the [Pantry](https://hackage.haskell.org/package/pantry) to
    specify the location of snapshots. Pantry is geared towards reproducible
    build plans with cryptographically secure specification of snapshots.

There are four ways to specify a snapshot location:

1.  Via a _convenience synonym_, which provides a short form for some common
    URLs (see further below).

    These are:

    *   **Stackage LTS Haskell snapshots**, for example:

        ~~~yaml
        snapshot: lts-24.30
        ~~~

        ??? info "Expansion of `lts-X.Y`"

            `lts-X.Y` is treated (by default) as:

             ~~~text
             github:commercialhaskell/stackage-snapshots:lts/X/Y.yaml
             ~~~

             and, consequently, expands to:

             ~~~text
             https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/X/Y.yaml
             ~~~

    *   **Stackage Nightly snapshots**, for example:

        ~~~yaml
        snapshot: nightly-2026-02-09
        ~~~

        ??? info "Expansion of `nightly-YYYY-MM-DD`"

            `nightly-YYYY-MM-DD` is treated (by default) as:

            ~~~text
            github:commercialhaskell/stackage-snapshots:nightly/YYYY/M/D.yaml
            ~~~

            and, consequently (see below), expands to:

            ~~~text
            https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/nightly/YYYY/M/D.yaml
            ~~~

    *   **GitHub**: `github:user/repo:path` is treated as:

        ~~~text
        https://raw.githubusercontent.com/user/repo/master/path
        ~~~

    ??? info "Overriding the default snapshot location base"

        By default, LTS Haskell and Stackage Nightly snapshot configurations are
        retrieved from the `stackage-snapshots` GitHub repository of user
        `commercialhaskell`. The
        [snapshot-location-base](../configure/yaml/non-project.md#snapshot-location-base)
        option allows a custom location to be set.

2.  Via a **compiler version**, for example:

    ~~~yaml
    snapshot: ghc-9.10.3
    ~~~

    This snapshot specifies only a compiler version and, indirectly, its boot
    packages.

3.  Via a **URL** pointing to a snapshot configuration file, for example:

    ~~~yaml
    snapshot: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/nightly/2025/8/17.yaml`
    ~~~

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

4.  Via a relative or absolute **local file path** pointing to a snapshot
    configuration file, for example:

    ~~~yaml
    snapshot: my-local-snapshot.yaml
    ~~~

    This can also be specified as:

    ~~~yaml
    snapshot:
      filepath: my-local-snapshot.yaml
    ~~~

    This allows local file paths and covenience synonyms to be disambiguated.

## At the command line

As in YAML configuration files, a snapshot location can be specified via a
convenience synoynm, a compiler version, a URL, or a local file path. In
addition, at the command line only:

*   `--snapshot lts-<major_version>` specifies the latest Stackage LTS Haskell
    snapshot with the specified major version;
*   `--snapshot lts` specifies, from those with the greatest major version, the
    latest Stackage LTS Haskell snapshot;
*   `--snapshot nightly` specifies the most recent Stackage Nightly snapshot;
    and
*   `--snapshot global` specifies the snapshot specified by the project-level
    configuration file in the `global-project` directory in the
    [Stack root](../topics/stack_root.md#global-project-directory).
