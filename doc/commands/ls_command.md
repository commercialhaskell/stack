<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack ls` commands

~~~text
stack ls COMMAND

Available commands:
  dependencies             View the dependencies
  globals                  View global packages
  snapshots                View snapshots (local by default)
  stack-colors             View Stack's output styles
  stack-colours            View Stack's output styles (alias for 'stack-colors')
  tools                    View Stack's installed tools
~~~

The `stack ls` commands list different types of information. Command `stack ls`
for the available commands.

## The `stack ls dependencies` command

Either

~~~text
stack ls dependencies COMMAND

Available commands:
  cabal                    Print dependencies as exact Cabal constraints
  json                     Print dependencies as JSON
  text                     Print dependencies as text (default)
  tree                     Print dependencies as tree
~~~

or

~~~text
stack ls dependencies [--[no-]license] [--separator SEP] [--filter ITEM]
                      [--[no-]external] [--[no-]include-base] [--depth DEPTH]
                      [--prune PACKAGES] [TARGET] [--flag PACKAGE:[-]FLAG]
                      [--test] [--bench] [--global-hints]
~~~

`stack ls dependencies` lists package versions used for a project.

By default:

*   with the `text` or `tree` subcommand (see below), the package name is
    followed by its version. Pass the `--license` flag to follow the package
    name with its licence. (Consistent with the Cabal package description format
    specification, only the American English spelling (license) is accepted.)
*   With the `text` or `tree` subcommand, the separator between the package name
    and what follows is a space character. Pass the `--separator` option to
    specify a different separator;
*   with the `text` command, all relevant package names are included. Pass the
    `--filter` option to specify an item to be filtered out from the results, if
    present. An item can be `$locals` (for all project packages) or a package
    name. It can be specified multiple times;

    !!! note

        The special value `$locals` will need to be enclosed with single quotes
        to distinguish it from a shell variable.

*   external dependencies are excluded from the output. Pass the flag
    `--external` to include external dependencies;
*   the `base` package and its dependencies are included in the output. Pass the
    flag `--no-include-base` to exclude `base` and its dependencies;
*   there is no limit to the depth of the resolution of dependencies. Pass the
    `--depth <depth>` option to limit the depth;
*   all relevant packages are included in the output. Pass the
    `--prune <packages>` option to exclude the specified packages (including
    project packages), where `<packages>` is a list of package names separated
    by commas;
*   for all relevant project packages, relevant dependencies are included in the
    output. However, each project package for which dependencies are included
    can be specified as a target argument. The argument uses the same format as
    the [`stack build` command](build_command.md) but components of project
    packages are ignored. Non-project packages are also ignored;

    !!! note

        If the first target is one of `cabal`, `json`, `text` and `tree`, then a
        subcommand must be specified.

*   test components of project packages are excluded from the output. Pass the
    flag `--test` to include test components;
*   benchmark components of project packages are excluded from the output. Pass
    the flag `--bench` to include benchmark components; and
*   global packages for the specified version of GHC are those specified by the
    global package database of an installed GHC. Pass the flag `--global-hints`
    to use a hint file for global packages. If a hint file is used, GHC does not
    need to be installed.

Subcommands specify the format of the output, as follows:

*   `cabal` lists the packages in the format of exact Cabal constraints.

    ~~~text
    stack ls dependencies cabal [--[no-]external] [--[no-]include-base]
                                [--depth DEPTH] [--prune PACKAGES] [TARGET]
                                [--flag PACKAGE:[-]FLAG] [--test] [--bench]
                                [--global-hints]
    ~~~

    For example (extract):

    ~~~text
    constraints:
    , Cabal ==3.6.3.0
    , Cabal-syntax ==3.6.0.0
    , Glob ==0.10.2
    ~~~

*   `json` lists dependencies in JSON format (an array of objects).

    ~~~text
    stack ls dependencies json [--[no-]external] [--[no-]include-base]
                               [--depth DEPTH] [--prune PACKAGES] [TARGET]
                               [--flag PACKAGE:[-]FLAG] [--test] [--bench]
                               [--global-hints]
    ~~~

    For example (extract):

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

*   `text` (the default) lists the packages, each on a separate line.

    ~~~text
    stack ls dependencies text [--[no-]license] [--separator SEP]
                               [--filter ITEM] [--[no-]external]
                               [--[no-]include-base] [--depth DEPTH]
                               [--prune PACKAGES] [TARGET]
                               [--flag PACKAGE:[-]FLAG] [--test] [--bench]
                               [--global-hints]
    ~~~

    For example (extract):

    ~~~text
    Cabal 3.6.3.0
    Cabal-syntax 3.6.0.0
    Glob 0.10.2
    ~~~

*   `tree` lists dependencies in the format of a tree.

    ~~~text
    stack ls dependencies tree [--[no-]license] [--separator SEP]
                               [--[no-]external] [--[no-]include-base]
                               [--depth DEPTH] [--prune PACKAGES] [TARGET]
                               [--flag PACKAGE:[-]FLAG] [--test] [--bench]
                               [--global-hints]
    ~~~

    For example (extract):

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

## The `stack ls globals` command

~~~text
stack ls globals [--[no-]global-hints]
~~~

`stack ls globals` will list all the global packages in alphabetical order.

By default:

* the global packages are those for the version of GHC specified by the snapshot
  according to a hints file. Pass the flag `--no-global-hints` to use the global
  package database of an installed GHC.

!!! note

    For example, on Windows, `stack ls globals` will include a version of the
    `Win32` and `unix` packages but `stack ls globals --no-global-hints` will
    exclude the `unix` package - and vice versa on Unix-like operating systems.

## The `stack ls snapshots` command

~~~text
stack ls snapshots [COMMAND] [-l|--lts] [-n|--nightly]

Available commands:
  local                    View local snapshots
  remote                   View remote snapshots
~~~

`stack ls snapshots` will list all the local snapshots by default. You can also
view the remote snapshots using `stack ls snapshots remote`. It also supports
options for viewing only lts (`-l`) and nightly (`-n`) snapshots.

## The `stack ls stack-colors` command

~~~text
stack ls stack-colors [--[no-]basic] [--[no-]sgr] [--[no-]example]
~~~

The British English spelling is also accepted (`stack ls stack-colours`).

`stack ls stack-colors` will list all of Stack's output styles. A number of
different formats for the output are available, see
`stack ls stack-colors --help`.

The default is a full report, with the equivalent SGR instructions and an
example of the applied style. The latter can be disabled with flags `--no-sgr`
and `--no-example`.

The flag `--basic` specifies a more basic report, in the format that is accepted
by Stack's command line option `--stack-colors` and the
[`stack-colors`](../configure/yaml/non-project.md#stack-colors) non-project
specific configuration option.

## The `stack ls tools` command

~~~text
stack ls tools [--filter TOOL_NAME]
~~~

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
