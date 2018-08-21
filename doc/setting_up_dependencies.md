# Dependency freezing

To make builds reproducible it makes sense to pin project dependencies to some
exact versions and this is what is stack's `freeze` command is about.

## Project freezing

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

## Snapshot freezing

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
