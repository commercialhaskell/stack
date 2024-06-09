<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack's environment variables

The environment variables listed in alphabetal order below can affect how Stack
behaves.

## `GH_TOKEN` or `GITHUB_TOKEN`

[:octicons-tag-24: 2.11.1](https://github.com/commercialhaskell/stack/releases/tag/v2.11.1)

Stack will use the value of the `GH_TOKEN` or, in the alternative,
`GITHUB_TOKEN` environment variable (if not an empty string) as credentials to
authenticate its requests of the GitHub REST API, using HTTP 'Basic'
authentication.

GitHub limits the rate of unauthenticated requests to its API, although most
users of Stack will not experience that limit from the use of Stack alone. The
limit for authenticated requests is significantly higher.

For more information about authentication of requests of the GitHub REST API,
see GitHub's REST API documentation.

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

## `STACK_CONFIG`

Related command: all commands that make use of Stack's
[global YAML configuration files](yaml_configuration.md) (`config.yaml`).

The environment variable `STACK_CONFIG` can be used to specify an absolute path
to the user-specific global YAML configuration file, overriding the default.

## `STACK_GLOBAL_CONFIG`

Related command: all commands that make use of Stack's
[global YAML configuration files](yaml_configuration.md) (`config.yaml`).

The environment variable `STACK_GLOBAL_CONFIG` can be used to specify an
absolute path to the system-wide global YAML configuration file, overriding the
default.

## `STACK_ROOT`

Related command: all commands that make use of Stack's
[user-specific global YAML configuration file](yaml_configuration.md)
(`config.yaml`).

Overridden by: Stack's global
[`--stack-root`](global_flags.md#-stack-root-option) option.

The environment variable `STACK_ROOT` can be used to specify the
[Stack root](stack_root.md) directory.

## `STACK_WORK`

Related command: all commands that make use of Stack's work directories.

Overridden by: Stack's [`work-dir`](yaml_configuration.md#work-dir) non-project
specific configuration option, or global
[`--work-dir`](global_flags.md#-work-dir-option) option.

The environment variable `STACK_WORK` can be used to specify the path of Stack's
work directory, within a local project or package directory, and override
Stack's default of `.stack-work`. The path must be a relative one, relative to
the root directory of the project or package. The relative path cannot include a
`..` (parent directory) component.

## `STACK_XDG`

Related command: all commands that make use of Stack's
[user-specific global YAML configuration file](yaml_configuration.md)
(`config.yaml`).

Overridden by: the use of Stack's `STACK_ROOT` environment variable, or the use
of Stack's global
[`--stack-root`](global_flags.md#-stack-root-option) option.

On Unix-like operating systems and Windows, Stack can be configured to follow
the XDG Base Directory Specification if the environment variable `STACK_XDG` is
set to any non-empty value.

## `STACK_YAML`

Related command: all commands that make use of Stack's
[project-level YAML configuration file](yaml_configuration.md).

Overridden by: Stack's global
[`--stack-yaml`](global_flags.md#-stack-yaml-option) option.

The environment variable `STACK_YAML` can be used to specify Stack's
project-level YAML configuration file.
