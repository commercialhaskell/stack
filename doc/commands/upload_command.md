<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack upload` command

~~~text
stack upload [ITEM] [-d|--documentation] [--pvp-bounds PVP-BOUNDS]
             [--ignore-check] [--[no-]test-tarball] [--tar-dir ARG]
             [--candidate] [--[no-]save-hackage-creds] [--setup-info-yaml URL]
             [--snapshot-location-base URL]
~~~

By default:

* the command uploads one or more packages. Pass the flag `--documentation`
  (`-d` for short) to upload documentation for one or more packages;

* the upload is a package to be published or documentation for a published
  package. Pass the flag `--candidate` to upload a
  [package candidate](http://hackage.haskell.org/upload#candidates) or
  documentation for a package candidate; and

* the command prompts to save the user's Hackage username and password in a
  local file. Pass the flag `--no-save-hackage-creds` to avoid the prompt.

At least one `ITEM` must be specified. For example, if the current working
directory is a package directory:

~~~text
stack upload .
~~~

## Upload one or more packages

Hackage accepts packages for uploading in a standard form, a compressed archive
('tarball') in the format produced by Cabal's `sdist` action.

If `ITEM` is a relative path to an sdist tarball, `stack upload` uploads the
package to Hackage.

If `ITEM` is a relative path to a package directory, `stack upload` generates a
file for your package, in the format accepted by Hackage for uploads, and
uploads the package to Hackage.

By default:

* the command will check each package for common mistakes. For further
  information, see the [`stack sdist` command](sdist_command.md) documentation.
  Pass the flag `--ignore-check` to disable such checks; and

* Stack will not test the resulting package archive. Pass the flag
  `--test-tarball` to cause Stack to test each resulting package archive, by
  attempting to build it.

The `--pvp-bounds <pvp_bounds_mode>` option determines whether and, if so, how
PVP version bounds should be added to the Cabal file of the package. The
available modes for basic use are: `none`, `lower`, `upper`, and `both`. The
available modes for use with Cabal file revisions are `lower-revision`,
`upper-revision` and `both-revision`.

For futher information, see the
[`pvp-bounds`](../configure/yaml/non-project.md#pvp-bounds) non-project
specific configuration option documentation.

The `--tar-dir <path_to_directory>` option determines whether the package
archive should be copied to the specified directory.

## Upload documentation for a package

:octicons-beaker-24: Experimental

[:octicons-tag-24: 2.15.1](https://github.com/commercialhaskell/stack/releases/tag/v2.15.1)

Hackage accepts documentation for a package for uploading in a standard form and
in a compressed archive ('tarball') in the `.tar.gz` format.

For further information about how to create such an archive file, see the
documentation for the
[`stack haddock --haddock-for-hackage`](build_command.md#-no-haddock-for-hackage-flag)
command.

If `ITEM` is a relative path to a package directory,
`stack upload <package_directory> --documentation` uploads an existing archive
file of documentation for the specified package to Hackage.

If the `--documentation` flag is passed then flags specific to package upload
are ignored.

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

=== "Windows"

    ~~~text
    $Env:HACKAGE_USERNAME='<username>'
    $Env:HACKAGE_PASSWORD='<password>'
    stack upload .
    ~~~

=== "Windows (Command Prompt)"

    ~~~text
    set HACKAGE_USERNAME=<username>
    set HACKAGE_PASSWORD=<password>
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

=== "Windows"

     ~~~text
     $Env:HACKAGE_KEY=<api_authentification_token>
     stack upload .
     ~~~

=== "Windows (Command Prompt)"

     ~~~text
     set HACKAGE_KEY=<api_authentification_token>
     stack upload .
     ~~~
