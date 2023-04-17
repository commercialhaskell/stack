<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack root location

The Stack root is a directory where Stack stores important files. The location
and contents of the directory depend on the operating system, whether
Stack is configured to use the XDG Base Directory Specification, and/or
whether an alternative location to Stack's default 'programs' directory has
been specified.

The location of the Stack root can be configured by setting the
[`STACK_ROOT`](environment_variables.md#stack_root) environment variable or
using Stack's [`--stack-root`](global_flags.md#stack-root-option) option on the
command line.

=== "Unix-like"

    The Stack root contains snapshot packages; (by default) tools such as GHC,
    in a `programs` directory; Stack's global
    [YAML configuration](yaml_configuration.md#yaml-configuration) file
    (`config.yaml`); and Stack's
    [`global-projects`](yaml_configuration.md#yaml-configuration) directory.

    The default Stack root is `~/.stack`.

=== "Windows"

    The Stack root contains snapshot packages; Stack's global
    [YAML configuration](yaml_configuration.md#yaml-configuration) file
    (`config.yaml`); and Stack's
    [`global-projects`](yaml_configuration.md#yaml-configuration) directory. The
    default location of tools such as GHC and MSYS2 is outside of the Stack
    root.

    The default Stack root is `%APPDIR%\stack`.

    If the `LOCALAPPDATA` environment variable exists, the default location of
    tools is `%LOCALAPPDATA%\Programs\stack`. Otherwise, it is the `programs`
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

    On Unix-like operating systems and Windows, Stack can be configured to
    follow the XDG Base Directory Specification if the environment variable
    `STACK_XDG` is set to any non-empty value. However, Stack will ignore that
    configuration if the Stack root location has been set on the command line or
    the `STACK_ROOT` environment variable exists.

    If Stack is following the XDG Base Directory Specification, the Stack root
    contains what it would otherwise contain for the operating system, but
    Stack's global YAML configuration file (`config.yaml`) may be located
    elsewhere.

    The Stack root is `<XDG_DATA_HOME>/stack`. If the `XDG_DATA_HOME`
    environment variable does not exist, the default is `~/.local/share/stack`
    on Unix-like operating systems and `%APPDIR%\stack` on Windows.

    The location of `config.yaml` is `<XDG_CONFIG_HOME>/stack`. If the
    `XDG_CONFIG_HOME` environment variable does not exist, the default is
    `~/.config/stack` on Unix-like operating systems and `%APPDIR%\stack` on
    Windows.

    This approach treats:

    *   the project-level YAML configuration file that is common to all projects
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
with the [`local-programs-path`](yaml_configuration.md#local-programs-path)
configuration option.

The location of the Stack root is reported by command:

~~~text
stack path --stack-root
~~~

The full path of Stack's global YAML configuration file is reported by command:

~~~text
stack path --global-config
~~~

The location of tools such as GHC for the current platform is reported by
command:

~~~text
stack path --programs
~~~
