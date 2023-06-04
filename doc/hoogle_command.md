<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack hoogle` command

~~~text
stack hoogle [-- ARGUMENT(S) (e.g. 'stack hoogle -- server --local')]
             [--[no-]setup] [--rebuild] [--server]
~~~

Hoogle is a Haskell API search engine. `stack hoogle` runs Hoogle. Stack needs
Hoogle version 5 or greater.

Stack will use a Hoogle database (`database.hoo`) specific to the project's
source map and the version of GHC, located in a subdirectory of subdirectory
`hoogle` of Stack's work directory for the project.

By default:

*   if a `hoogle` executable is found on the `PATH`, Stack will try to use it.
    Otherwise, Stack will try to identify an executable as a build target. If
    the Hoogle database does not exist, Stack will generate it with
    `hoogle generate --local`. `hoogle generate --local` queries `ghc-pkg` and
    generates links for all packages which have documentation and Hoogle input
    files (`*.txt`) generated. Pass the flag `--no-setup` to skip such setup;

*   the existing Hoogle database is used. Pass the flag `--rebuild` to trigger
    the generation of a new Hoogle database (generated as above); and

*   `hoogle` is passed the specified arguments (if any). The arguments are
    usually the subject of the search. Pass the flag `--server` to first pass
    `server --local --port 8080` before those arguments.
    `hoogle server --local --port 8080` starts a local Hoogle web server, using
    port 8080, that allows the following of `file://` links.
