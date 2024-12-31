<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack root

The Stack root is a directory where Stack stores important files.

On Unix-like operating systems and Windows, Stack can be configured to follow
the XDG Base Directory Specification if the environment variable `STACK_XDG` is
set to any non-empty value. However, Stack will ignore that configuration if the
Stack root location has been set on the command line or the `STACK_ROOT`
environment variable exists.

## Location

The location of the Stack root depends on the operating system, whether Stack is
configured to use the XDG Base Directory Specification, and/or whether an
alternative location to Stack's default 'programs' directory has been specified.

The location of the Stack root can be configured by setting the
[`STACK_ROOT`](../configure/environment_variables.md#stack_root) environment
variable or using Stack's
[`--stack-root`](../configure/global_flags.md#-stack-root-option) option on the
command line.

=== "Unix-like"

    The Stack root contains snapshot packages; (by default) tools such as GHC,
    in a `programs` directory; Stack's global
    [configuration](../configure/yaml/index.md) file (`config.yaml`); and
    Stack's [`global-projects`](../configure/yaml/index.md) directory.

    The default Stack root is `~/.stack`.

=== "Windows"

    The default Stack root is `$Env:APPDATA\stack`.

    If the `LOCALAPPDATA` environment variable exists, then the default location
    of tools is `$Env:LOCALAPPDATA\Programs\stack`. Otherwise, it is the
    `programs` directory in the Stack root.

    !!! warning

        If there is a space character in the `$Env:LOCALAPPDATA` path (which may
        be the case if the relevant user account name and its corresponding user
        profile path have a space) this may cause problems with building
        packages that make use of the GNU project's `autoconf` package and
        `configure` shell script files. That may be the case particularly if
        there is no corresponding short name ('8 dot 3' name) for the directory
        in the path with the space (which may be the case if '8 dot 3' names
        have been stripped or their creation not enabled by default). If there
        are problems building, it will be necessary to override the default
        location of Stack's 'programs' directory to specify an alternative path
        that does not contain space characters. Examples of packages on
        Hackage that make use of `configure` are `network` and `process`.

    On Windows, the length of filepaths may be limited (to
    [MAX_PATH](https://docs.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation?tabs=cmd)),
    and things can break when this limit is exceeded. Setting a Stack root with
    a short path to its location (for example, `C:\sr`) can help.

=== "Windows (Command Prompt)"

    The default Stack root is `%APPDATA%\stack`.

    If the `LOCALAPPDATA` environment variable exists, then the default location
    of tools is `%LOCALAPPDATA%\Programs\stack`. Otherwise, it is the `programs`
    directory in the Stack root.

    !!! warning

        If there is a space character in the `%LOCALAPPDATA%` path (which may be
        the case if the relevant user account name and its corresponding user
        profile path have a space) this may cause problems with building
        packages that make use of the GNU project's `autoconf` package and
        `configure` shell script files. That may be the case particularly if
        there is no corresponding short name ('8 dot 3' name) for the directory
        in the path with the space (which may be the case if '8 dot 3' names
        have been stripped or their creation not enabled by default). If there
        are problems building, it will be necessary to override the default
        location of Stack's 'programs' directory to specify an alternative path
        that does not contain space characters. Examples of packages on
        Hackage that make use of `configure` are `network` and `process`.

    On Windows, the length of filepaths may be limited (to
    [MAX_PATH](https://docs.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation?tabs=cmd)),
    and things can break when this limit is exceeded. Setting a Stack root with
    a short path to its location (for example, `C:\sr`) can help.

=== "XDG Base Directory Specification"

    The Stack root is `<XDG_DATA_HOME>/stack`. If the `XDG_DATA_HOME`
    environment variable does not exist, the default is `~/.local/share/stack`
    on Unix-like operating systems and `%APPDATA%\stack` on Windows.

    The location of `config.yaml` is `<XDG_CONFIG_HOME>/stack`. If the
    `XDG_CONFIG_HOME` environment variable does not exist, the default is
    `~/.config/stack` on Unix-like operating systems and `%APPDATA%\stack` on
    Windows.

    This approach treats:

    *   the project-level configuration file that is common to all projects
        without another such file in their project directory or its ancestor
        directories as _data_ rather than as part of Stack's own
        _configuration_;

    *   the snapshots database as essential data rather than as non-essential
        data that would be part of a _cache_, notwithstanding that Stack will
        rebuild that database as its contents are needed; and

    *   the Pantry store as essential data rather than as non-essential data
        that would be part of a _cache_, notwithstanding that Stack will
        download the package index and rebuild the store if it is absent.

An alternative to the default location of tools such as GHC can be specified
with the
[`local-programs-path`](../configure/yaml/non-project.md#local-programs-path)
configuration option.

The location of the Stack root is reported by command:

~~~text
stack path --stack-root
~~~

The full path of Stack's global configuration file is reported by command:

~~~text
stack path --global-config
~~~

The location of tools such as GHC for the current platform is reported by
command:

~~~text
stack path --programs
~~~

## Contents

The contents of the Stack root depend on the operating system, whether Stack is
configured to use the XDG Base Directory Specification, and/or whether an
alternative location to Stack's default 'programs' directory has been specified.

=== "Unix-like"

    The Stack root contains snapshot packages; (by default) tools such as GHC,
    in a `programs` directory; Stack's global
    [configuration](../configure/yaml/index.md) file (`config.yaml`); and
    Stack's [`global-projects`](../configure/yaml/index.md) directory.

=== "Windows"

    The Stack root contains snapshot packages; Stack's global
    [configuration](../configure/yaml/index.md) file (`config.yaml`); and
    Stack's [`global-projects`](../configure/yaml/index.md) directory. The
    default location of tools such as GHC and MSYS2 is outside of the Stack
    root.

=== "XDG Base Directory Specification"

    If Stack is following the XDG Base Directory Specification, the Stack root
    contains what it would otherwise contain for the operating system, but
    Stack's global configuration file (`config.yaml`) may be located elsewhere.

### `config.yaml`

This is Stack's global configuration file. For further information, see the
documentation for non-project specific
[configuration](../configure/yaml/non-project.md#non-project-specific-configuration).

If the file is deleted, and Stack needs to consult it, then Stack will create a
file with default contents.

### `stack.sqlite3`

This is a 'user' database that Stack uses to cache certain information. The
associated lock file is `stack.sqlite3.pantry-write-lock`.

### `.stack-work` directory (optional)

Stack can build when there is no project-level configuration file (including one
in the `global-project` directory of the Stack root); for example, as a result
of a [`stack script`](../commands/script_command.md) command (at the command
line or in a [Stack interpreter options comment](scripts.md) in a Haskell script
file). When it does so, the directory corresponding to a project directory is
the Stack root. Stack will create its work directory, named `.stack-work` by
default, in the Stack root.

If the work directory is deleted, and Stack needs that work directory, then
Stack will recreate it.

### `global-project` directory

This contains:

* an explanation of the directory (`README.txt`);
* the project-level configuration file (`stack.yaml`) for the global project
  and its associated lock file (`stack.yaml.lock`); and
* if created, Stack's working directory (`.stack-work`) for the global project.

If the project-level configuration file is deleted, and Stack needs to consult
it, then Stack will recreate the contents of the directory.

### `pantry\hackage` directory

This contains a local cache of the package index. If the contents of the
directory are deleted, and Stack needs to consult the package index, then Stack
will seek to download the latest package index.

!!! info

    Stack depends on package `pantry` which, in turn, depends on package
    `hackage-security`. The latter handles the local cache of the package index.
    The type `CacheLayout` represents the location of the files that are cached.
    `pantry` uses `cabalCacheLayout :: CacheLayout`, the layout that Cabal (the
    tool) uses. That is what specifies the names of the files used to cache the
    package index, including `00-index.tar` and `00-index.tar.gz`.

### `pantry` directory

This contains:

* the Pantry database used by Stack (`pantry.sqlite3`) and its associated lock
  file (`pantry.sqlite2.pantry-write-lock`). If the database is deleted, and
  Stack needs to consult it, then Stack will seek to create and initialise it.
  The database is initialised with information from the package index; and
* a database of package versions that come with each version of GHC
  (`global-hints-cache.yaml`).

### `programs` directory

This contains a directory for the platform. That directory contains for each
installed Stack-supplied tool:

* the archive file for the tool. This can be deleted;
* a file indicating the tool is installed (`<tool_name>.installed`); and
* a directory for the tool.

To remove a Stack-supplied tool, delete all of the above. If Stack needs a
Stack-supplied tool and it is unavailable, then Stack will seek to obtain it.

### `scripts` directory (optional)

If the `--compile` or `--optimize` and `--use-root` flags are used with the
[`stack script`](../commands/script_command.md) command, then this contains:

* script-specific locations, each containing all the compilation outputs
  (inclduing the executable) generated by the command.

If the `scripts` directory, or a script-specific location within it, is deleted,
and Stack needs that directory, then Stack will recreate it.

### `setup-exe-cache` directory

This contains a directory for the platform. That directory contains, for each
version of GHC (an associated version of Cabal (the library)) that Stack has
used, an executable that Stack uses to access Cabal (the library).

If the contents of the directory are deleted, and Stack needs the executable,
then Stack will seek to rebuild it.

### `setup-exe-src` directory

See the documentation for the
[`setup-exe-cache` directory](#setup-exe-cache-directory). This contains the two
source files (`setup-<hash>.hs` and `setup-shim-<hash>.hs`) that Stack uses to
build the executable.

If the contents of the directory are deleted, and Stack needs the executable,
then Stack will recreate them.

The hash in the names of the source files is a hash of arguments passed to GHC
when building the executable and the contents of the two source files.

The content of the `setup-<hash>.hs` file is the familiar:

~~~haskell
import Distribution.Simple
main = defaultMain
~~~

The content of the `setup-shim-<hash>.hs` file uses `main` except when the
executable is called with arguments `repl` and `stack-initial-build-steps`. Then
Stack uses Cabal (the library) to create the autogenerated files for every
configured component. Stack's `stack ghci` or `stack repl` commands call the
executable with those arguments.

### `snapshots` directory

This contains a directory for each snapshot that Stack creates when building
immutable dependencies of projects.

If the contents of the directory are deleted, and the snapshot is not available
to Stack when it builds, then Stack will recreate the snapshot.

### `templates` directory

This contains a `.hsfile` for each project template that Stack has used. For
further information, see the
[`stack templates`](../commands/templates_command.md) command documentation.

If the contents of the directory are deleted, an Stack needs a project template,
then Stack will seek to download the template.

### `upload` directory

This may contain saved credentials for uploading packages to Hackage
(`credentials.json`).
