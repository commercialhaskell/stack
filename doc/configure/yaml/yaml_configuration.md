<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Configuration

Stack is configured by the content of files in the [YAML](https://yaml.org/)
format.

## Project-specific and non-project specific options

Stack's configuration options are each of one of two types:

<div class="grid cards" markdown>

-   :material-account:{ .lg .middle } __Project-specific__

    Configured at the project level

    ---

    [:octicons-arrow-right-24: Learn more](project.md)

-   :material-account-multiple:{ .lg .middle } __Non-project specific__

    Configured globally or at the project level.

    ---

    [:octicons-arrow-right-24: Learn more](non-project.md)

</div>

## Project-level and global configuration files

Stack's configuration files are each of one of two types:

<div class="grid cards" markdown>

-   :material-language-haskell:{ .lg .middle } __Project-level__

    Named `stack.yaml` by default.

    ---

    Contains [project-specific](project.md) options and may contain
    [non-project-specific](non-project.md) options.

    Non-project-specific options in the project-level configuration file in the
    `global-project` directory (see below) are ignored by Stack.

-   :octicons-globe-24:{ .lg .middle } __Global__

    Named `config.yaml`.

    There is a user-specific file and there may be a system-wide one. If a
    user-specific file does not exist, then Stack will create one.

    ---

    Contains [non-project-specific](non-project.md) options.

    An option set in the user-specific file will override a corresponding option
    set in the system-wide file (if it exists).

</div>

## Location of project-level configuration

Stack obtains project-level configuration from one of the following (in order of
preference):

1. A file specified by the `--stack-yaml` command line option.
2. A file specified by the `STACK_YAML` environment variable.
3. A file named `stack.yaml` in the current directory or an ancestor directory.
4. A file name `stack.yaml` in the `global-project` directory in the
   [Stack root](../../topics/stack_root.md).

## Location of global configuration

The default location of global configuration files depends on the operating
system and, in the case of the user-specific file, whether Stack is configured
to use the XDG Base Directory Specification.

An absolute path to these files can be specified by the
[`STACK_CONFIG`](../environment_variables.md#stack_config) and
[`STACK_GLOBAL_CONFIG`](../environment_variables.md#stack_config) environment
variables, respectively.

=== "Unix-like"

    The default locations are:

    * system-wide: `/etc/stack/config.yaml`; and
    * user-specific: `config.yaml` in the
      [Stack root](../../topcis/stack_root.md).

    !!! note

        For compatibility with Stack 0.1.5.0 and earlier, if deprecated file
        `/etc/stack/config` exists, then Stack will use it instead of
        `/etc/stack/config.yaml`.

=== "Windows"

    The default locations are:

    * system-wide: none; and
    * user-specific: `config.yaml` in the
      [Stack root](../../topics/stack_root.md).

=== "XDG Base Directory Specification"

    On Unix-like operating systems and Windows, Stack can be configured to
    follow the XDG Base Directory Specification if the environment variable
    `STACK_XDG` is set to any non-empty value. However, Stack will ignore that
    configuration if the [Stack root](../../topics/stack_root.md) location has
    been set on the command line or the `STACK_ROOT` environment variable
    exists.

    If Stack is following the XDG Base Directory Specification, the location of
    `config.yaml` (for user-specific options) is `<XDG_CONFIG_HOME>/stack`. If
    the `XDG_CONFIG_HOME` environment variable does not exist, the default is
    `~/.config/stack` on Unix-like operating systems and `%APPDIR%\stack` on
    Windows.
