<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Configuration and customisation

Stack is configured by the content of YAML files.

!!! info

    A Haskell package is an organised collection of Haskell code and related
    files. It is described by a Cabal file or a `package.yaml` file (which can
    be used to generate a Cabal file). The package description is itself part of
    the package. Its file is located in the root directory of a project package
    or dependency located locally.

    A Stack project is a local directory that contains a Stack project-level
    configuration file (`stack.yaml`, by default). A project may relate to more
    than one project package. A single-package project's directory will usually
    also be the project package's root directory.

## YAML configuration

Stack's YAML configuration options break down into
[project-specific](project.md) options and
[non-project-specific](non-project.md) options. The former are configured at the
project level. The latter are configured at the project level or globally.

The **project-level** configuration file (`stack.yaml`, by default) contains
project-specific options and may contain non-project-specific options. However,
non-project-specific options in the project-level configuration file in the
`global-project` directory (see below) are ignored by Stack.

Stack obtains project-level configuration from one of the following (in order of
preference):

1. A file specified by the `--stack-yaml` command line option.
2. A file specified by the `STACK_YAML` environment variable.
3. A file named `stack.yaml` in the current directory or an ancestor directory.
4. A file name `stack.yaml` in the `global-project` directory in the
   [Stack root](../../topics/stack_root.md).

The **global** configuration files (`config.yaml`) contain only
non-project-specific options. There is a user-specific global confguration file
and there may be an optional system-wide global configuration file. If a
user-specific global configuration file does not exist, then Stack will create
one. An option set in the user-specific file will override a corresponding
option set in the system-wide file (if it exists).

The default location of these files depends on the operating system and, in the
case of the user-specific file, whether Stack is configured to use the XDG Base
Directory Specification. An absolute path to these files can be specified by the
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

This page is intended to document fully all YAML configuration options. If you
identify any inaccuracies or incompleteness, please update the page, and if
you're not sure how, open an issue labeled "question".

If you wish to understand the difference between a `stack.yaml` files and a
Cabal file (named `<package_name>.cabal`), see the
[stack.yaml vs a Cabal file](../../topics/stack_yaml_vs_cabal_package_file.md)
documentation.
