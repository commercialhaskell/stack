<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack ls` commands

~~~text
stack ls COMMAND

Available commands:
  dependencies             View the dependencies
  snapshots                View snapshots (local by default)
  stack-colors             View Stack's output styles
  stack-colours            View Stack's output styles (alias for 'stack-colors')
  tools                    View Stack's installed tools
~~~

The `stack ls` commands list different types of information. See `stack ls` for
the available commands.

## The `stack ls dependencies` command

`stack ls dependencies` lists all of the packages and versions used for a
project. All local packages are considered by default, but a target can be
specified as an argument. For further information, see the
[target syntax](build_command.md#target-syntax) documentation.

Subcommands specify the format of the output, as follows:

* `text` (the default) lists the packages, each on a separate line. For example
  (extract):

    ~~~text
    Cabal 3.6.3.0
    Cabal-syntax 3.6.0.0
    Glob 0.10.2
    ~~~

* `cabal` lists the packages in the format of exact Cabal constraints.
  For example (extract):

    ~~~text
    constraints:
    , Cabal ==3.6.3.0
    , Cabal-syntax ==3.6.0.0
    , Glob ==0.10.2
    ~~~

* `tree` lists dependencies in the format of a tree. For example (extract):

    ~~~text
    Packages
    └─┬ stack 2.10.0
      ├─┬ Cabal 3.6.3.0
      │ ├─┬ Win32 2.12.0.1
      │ │ ├─┬ base 4.16.3.0
      │ │ │ ├─┬ ghc-bignum 1.2
      │ │ │ │ └─┬ ghc-prim 0.8.0
      │ │ │ │   └── rts 1.0.2
      │ │ │ ├─┬ ghc-prim 0.8.0
    ~~~

* `json` lists dependencies in JSON format (an array of objects). For example
  (extract):

    ~~~text
    [{"dependencies":["base","bytestring"],"license":"BSD3","location":{"type":"hackage","url":"https://hackage.haskell.org/package/zlib-0.6.3.0"},"name":"zlib","version":"0.6.3.0"},
    ~~~

  Each object has the following keys:

    ~~~json
    name: zlib
    version: 0.6.3.0
    location:
      type: hackage
      url: https://hackage.haskell.org/package/zlib-0.6.3.0
    licence: BSD3
    dependencies:
    - base
    - bytestring
    ~~~

The `--separator` option specifies the separator between the package name and
its version. The default is a space character.

Set the `--licence` flag, after the `text` or `tree` subcommand, to replace each
package's version with its licence.

Set the `--no-external` flag to exclude external dependencies.

Set the `--no-include-base` flag to exclude dependencies on the `base` package.

The `--depth` option limits the depth of dependency resolution.

The `--prune <packages>` option prunes the specified packages from the output,
where `<packages>` is a comma separated list of package names.

The `--flag` option allows Cabal flags to be specified.

Pass the `--test` flag to consider the dependencies of test suite components.

Pass the `--bench` flag to consider the dependencies of benchmark components.

Pass the `--global-hints` flag to use a hints file for global packages. The
command then does not require an installed GHC.

## The `stack ls snapshots` command

`stack ls snapshots` will list all the local snapshots by default. You can also
view the remote snapshots using `stack ls snapshots remote`. It also supports
options for viewing only lts (`-l`) and nightly (`-n`) snapshots.

## The `stack ls stack-colors` command

The British English spelling is also accepted (`stack ls stack-colours`).

`stack ls stack-colors` will list all of Stack's output styles. A number of
different formats for the output are available, see
`stack ls stack-colors --help`.

The default is a full report, with the equivalent SGR instructions and an
example of the applied style. The latter can be disabled with flags `--no-sgr`
and `--no-example`.

The flag `--basic` specifies a more basic report, in the format that is accepted
by Stack's command line option `--stack-colors` and the YAML configuration key
`stack-colors`.

## The `stack ls tools` command

`stack ls tools` will list Stack's installed tools. On Unix-like operating
systems, they will be one or more versions of GHC. On Windows, they will include
MSYS2. For example, on Windows the command:

~~~text
stack ls tools
~~~

yields output like:

~~~text
ghc-9.4.1
ghc-9.2.4
ghc-9.0.2
msys2-20210604
~~~

The `--filter <tool_name>` option will filter the output by a tool name (e.g.
'ghc', 'ghc-git' or 'msys2'). The tool name is case sensitive. For example the
command:

~~~text
stack ls tools --filter ghc
~~~

yields output like:

~~~text
ghc-9.4.1
ghc-9.2.4
ghc-9.0.2
~~~
