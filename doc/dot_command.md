<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack dot` command

~~~text
stack dot [--[no-]external] [--[no-]include-base] [--depth DEPTH]
          [--prune PACKAGES] [TARGET] [--flag PACKAGE:[-]FLAG]
          [--test] [--bench] [--global-hints]
~~~

A package and its dependencies and the direct dependency relationships between
them form a directed graph. [Graphviz](https://www.graphviz.org/) is open source
software that visualises graphs. It provides the DOT language for defining
graphs and the `dot` executable for drawing directed graphs. Graphviz is
available to [download](https://www.graphviz.org/download/) for Linux, Windows,
macOS and FreeBSD.

`stack dot` produces output, to the standard output channel, in the DOT language
to represent the relationships between your packages and their dependencies.

By default:

* external dependencies are excluded from the output. Pass the flag
  `--external` to include external dependencies;
* the `base` package and its dependencies are included in the output. Pass the
  flag `--no-include-base` to exclude `base` and its dependencies;
* there is no limit to the depth of the resolution of dependencies. Pass the
  `--depth <depth>` option to limit the depth;
* all relevant packages are included in the output. Pass the
  `--prude <packages>` option to exclude the specified packages, where
  `<packages>` is a list of package names separated by commas;
* all packages in the project are included in the output. However, the target
  for the command can be specified as an argument. It uses the same format
  as the [`stack build` command](build_command.md);
* test components of the packages in the project are excluded from the output.
  Pass the flag `--test` to include test components; and
* benchmark components of the packages in the project are excluded from the
  output. Pass the flag `--bench` to include benchmark components.git p

Pass the option `--flag <package_name>:<flag_name>` or
`--flag <package_name>:-<flag_name>` to set or unset a Cabal flag. This
option can be specified multiple times.

Pass the flag `--global-hints` to use a hint file for global packages. If a hint
file is used, GHC does not need to be installed.

## Examples

The following examples are based on a version of the
[`wreq` package](https://hackage.haskell.org/package/wreq). In each case, the
output from `stack dot` is piped as an input into Graphviz's `dot` executable,
and `dot` produces output in the form of a PNG file named `wreq.png`.

*   A simple example:

        ~~~text
        stack dot | dot -Tpng -o wreq.png
        ~~~

    [![wreq](https://cloud.githubusercontent.com/assets/591567/8478591/ae10a418-20d2-11e5-8945-55246dcfac62.png)](https://cloud.githubusercontent.com/assets/591567/8478591/ae10a418-20d2-11e5-8945-55246dcfac62.png)

*   Include external dependencies:

        ~~~text
        stack dot --external | dot -Tpng -o wreq.png
        ~~~

    [![wreq_ext](https://cloud.githubusercontent.com/assets/591567/8478621/d247247e-20d2-11e5-993d-79096e382abd.png)](https://cloud.githubusercontent.com/assets/591567/8478621/d247247e-20d2-11e5-993d-79096e382abd.png)

*   Include external dependencies, limit the depth and save the output from
    `stack dot` as an intermediate file (`wreq.dot`).

        ~~~text
        stack dot --external --depth 1 > wreq.dot
        dot -Tpng -o wreq.png wreq.dot
        ~~~

*   Include external dependencies, exclude `base` and limit the depth:

        ~~~text
        stack dot --no-include-base --external --depth 1 | dot -Tpng -o wreq.png
        ~~~

    [![wreq_depth](https://cloud.githubusercontent.com/assets/591567/8484310/45b399a0-20f7-11e5-8068-031c2b352961.png)](https://cloud.githubusercontent.com/assets/591567/8484310/45b399a0-20f7-11e5-8068-031c2b352961.png)

*   Include external dependencies and prune `base` and other packages:

        ~~~text
        stack dot --external --prune base,lens,wreq-examples,http-client,aeson,tls,http-client-tls,exceptions | dot -Tpng -o wreq.png
        ~~~

    [![wreq_pruned](https://cloud.githubusercontent.com/assets/591567/8478768/adbad280-20d3-11e5-9992-914dc24fe569.png)](https://cloud.githubusercontent.com/assets/591567/8478768/adbad280-20d3-11e5-9992-914dc24fe569.png)

*   Include external dependencies, prune `base` and other packages, and use a
    different Graphviz executable to draw the graph:

    Graphviz's `twopi` executable draws graphs in a radial layout.

        ~~~text
        stack dot --external --prune base,lens,wreq-examples,http-client,aeson,tls,http-client-tls,exceptions | twopi -Groot=wreq -Goverlap=false -Tpng -o wreq.png
        ~~~

    [![wreq_pruned](https://cloud.githubusercontent.com/assets/591567/8495538/9fae1184-216e-11e5-9931-99e6147f8aed.png)](https://cloud.githubusercontent.com/assets/591567/8495538/9fae1184-216e-11e5-9931-99e6147f8aed.png)
