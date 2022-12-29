<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack upload` command

~~~text
stack upload [DIR] [--pvp-bounds PVP-BOUNDS] [--ignore-check]
             [--[no-]test-tarball] [--tar-dir ARG] [--candidate]
~~~

Hackage accepts packages for uploading in a standard form, a compressed archive
('tarball') in the format produced by Cabal's `sdist` action.

`stack upload` generates a file for your package, in the format accepted by
Hackage for uploads, and uploads the package to Hackage. For example, if the
current working directory is the root directory of your project:

~~~text
stack upload .
~~~

## The `HACKAGE_USERNAME` and `HACKAGE_PASSWORD` environment variables

[:octicons-tag-24: 2.3.1](https://github.com/commercialhaskell/stack/releases/tag/v2.3.1)

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

## The `HACKAGE_KEY` environment variable

[:octicons-tag-24: 2.7.5](https://github.com/commercialhaskell/stack/releases/tag/v2.7.5)

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

## `--candidate` flag

Pass the flag to upload a
[package candidate](http://hackage.haskell.org/upload#candidates).

## `--ignore-check` flag

Pass the flag to disable checks of the package for common mistakes. By default,
the command will check the package for common mistakes.

## `--pvp-bounds` option

The `--pvp-bounds <pvp_bounds_mode>` option determines whether and, if so, how
PVP version bounds should be added to the Cabal file of the package. The
available modes for basic use are: `none`, `lower`, `upper`, and `both`. The
available modes for use with Cabal file revisions are `lower-revision`,
`upper-revision` and `both-revision`.

For futher information, see the
[YAML configuration](yaml_configuration.md#pvp-bounds) documentation.

## `--tar-dir` option

The `--tar-dir <path_to_directory>` option determines whether the package
archive should be copied to the specified directory.

## `--[no-]test-tarball` flag

Default: Disabled

Set the flag to cause Stack to test the resulting package archive, by attempting
to build it.
