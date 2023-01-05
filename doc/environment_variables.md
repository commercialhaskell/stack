<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack's environment variables

The environment variables listed in alphabetal order below can affect how Stack
behaves.

## `HACKAGE_KEY`

[:octicons-tag-24: 2.7.5](https://github.com/commercialhaskell/stack/releases/tag/v2.7.5)

Related command: [`stack upload`](upload_command.md)

Hackage allows its members to register an API authentification token and to
authenticate using the token.

A Hackage API authentification token can be used with `stack upload` instead of
username and password, by setting the `HACKAGE_KEY` environment variable. For
example:

=== "Unix-like"

     ~~~text
     HACKAGE_KEY=<api_authentification_token>
     stack upload .
     ~~~

=== "Windows (with PowerShell)"

     ~~~text
     $Env:HACKAGE_KEY=<api_authentification_token>
     stack upload .
     ~~~

## `HACKAGE_USERNAME` and `HACKAGE_PASSWORD`

[:octicons-tag-24: 2.3.1](https://github.com/commercialhaskell/stack/releases/tag/v2.3.1)

Related command: [`stack upload`](upload_command.md)

`stack upload` will request a Hackage username and password to authenticate.
This can be avoided by setting the `HACKAGE_USERNAME` and `HACKAGE_PASSWORD`
environment variables. For
example:

=== "Unix-like"

    ~~~text
    export $HACKAGE_USERNAME="<username>"
    export $HACKAGE_PASSWORD="<password>"
    stack upload .
    ~~~

=== "Windows (with PowerShell)"

    ~~~text
    $Env:HACKAGE_USERNAME='<username>'
    $Env:HACKAGE_PASSWORD='<password>'
    stack upload .
    ~~~

## `NO_COLOR`

Related command: all commands that can produce colored output using control character sequences.

Stack follows the standard at http://no-color.org/. Stack checks for a
`NO_COLOR` environment variable. When it is present and not an empty string
(regardless of its value), Stack prevents the addition of control character
sequences for color to its output.

## `STACK_ROOT`

Related command: all commands that make use of Stack's global YAML configuration
file (`config.yaml`).

Overridden by: Stack's global
[`--stack-root`](global_flags.md#the---stack-root-option) option.

The environment variable `STACK_ROOT` can be used to specify the Stack root
directory.

## `STACK_WORK`

Related command: all commands that make use of Stack's work directories.

Overridden by: Stack's [`work-dir`](yaml_configuration.md#work-dir) non-project
specific configuration option, or global
[`--work-dir`](global_flags.md#the---work-dir-option) option.

The environment variable `STACK_WORK` can be used to specify the path of Stack's
work directory, within a local project or package directory, and override
Stack's default of `.stack-work`. The path must be a relative one, relative to
the root directory of the project or package. The relative path cannot include a
`..` (parent directory) component.

## `STACK_XDG`

Related command: all commands that make use of Stack's user-specific general
YAML configuration file (`config.yaml`).

Overridden by: the use of Stack's `STACK_ROOT` environment variable, or the use
of Stack's global
[`--stack-root`](global_flags.md#the---stack-root-option) option.

On Unix-like operating systems and Windows, Stack can be configured to follow
the XDG Base Directory Specification if the environment variable `STACK_XDG` is
set to any non-empty value.

## `STACK_YAML`

Related command: all commands that make use of Stack's project-level YAML
configuration file.

Overridden by: Stack's global
[`--stack-yaml`](global_flags.md#the---stack-yaml-option) option.

The environment variable `STACK_YAML` can be used to specify Stack's
project-level YAML configuration file.
