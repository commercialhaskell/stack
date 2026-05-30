<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Stack environment

Command
[`stack config env`](../commands/config_command.md#the-stack-config-env-command)
or [`stack path --bin-path`](../commands/path_command.md) to see how the PATH is
defined in the Stack environment.

In that environment, Stack adds certain directories to the start of the PATH.
The directories added are set out below, in order of search priority.

!!! note

    Stack does not automatically include its
    [local binary directory](../configure/yaml/non-project.md#local-bin-path) on
    the PATH in the Stack environment. Command
    [`stack path --local-bin`](../commands/path_command.md) to see that
    directory.

## The project's `bin` directory

The Stack [work directory](stack_work.md#project-stack-work-directory) for a
project contains an `install` directory with a path to a directory (the local
install root directory) containing a `bin` directory, containing built
executable files. Command
[`stack path --local-install-root`](../commands/path_command.md) to see the
local install root directory.

!!! info

    Stack's [`build` command](../commands/build_command.md) installs built
    executable files of project packages in this directory. Such files can be
    executed using Stack's [`exec` command](../commands/exec_command.md).

## The snapshot's `bin` directory

The `bin` directory in the root directory for snapshot installation. Command
[`stack path --snapshot-install-root`](../commands/path_command.md) to see that
root directory.

## The compiler tools directory

When the directory is required or requested, Stack creates, in the
[Stack root](stack_root.md#compiler-tools-directory-optional), a compiler tools
directory for the specified compiler version. Command
[`stack path --compiler-tools-bin`](../commands/path_command.md) to see that
directory.

!!! info

    Stack's
    [`build` command](../commands/build_command.md#-no-copy-compiler-tool-flag)
    can be configured to install built executable files of project packages into
    the
    [compiler tools directory](stack_root.md#compiler-tools-directory-optional)
    for the specified compiler version.

!!! info

    If you wish to override a tool that is co-located with the specified
    compiler's executable file, put a copy of the alternative tool (or a link to
    it) in the
    [compiler tools directory](stack_root.md#compiler-tools-directory-optional)
    for the specified compiler version. If that directory does not exist, it can
    be created with Stack's
    [`config compiler-tools` command](../commands/config_command.md#the-stack-config-compiler-tools-command).

## The compiler binary's directory

The directory in which the GHC executable file (binary) that will be used by
Stack is located. Command
[`stack path --compiler-bin`](../commands/path_command.md) to see that
directory.

## GHC's MinGW `bin` directory (Windows only)

The GHC-supplied MinGW `bin` directory.

## Stack-supplied MSYS2 environment `bin` directory (Windows only)

The `bin` directory for the specified MSYS2 environment of the
[Stack-supplied MSYS2](developing_on_windows.md).

## Stack-supplied MSYS2 `usr\local\bin` directory (Windows only)

The `usr\local\bin` directory for the
[Stack-supplied MSYS2](developing_on_windows.md).

## Stack-supplied MSYS2 `usr\bin` directory (Windows only)

The `usr\bin` directory for the
[Stack-supplied MSYS2](developing_on_windows.md).

## Specified extra paths

Any extra paths that have been configured using Stack's
[`extra-paths`](../configure/yaml/non-project.md#extra-path) configuration
option.
