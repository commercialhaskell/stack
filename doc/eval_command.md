<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack eval` command

~~~text
stack eval CODE [--[no-]ghc-package-path] [--[no-]stack-exe]
           [--package PACKAGE] [--rts-options RTSFLAG] [--cwd DIR]
~~~

GHC has an
[expression-evaluation mode](https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#eval-mode),
set by passing the GHC option
`-e <expression>`. Commanding `stack eval <code>` is equivalent to commanding:

~~~text
stack exec ghc -- -e <code>
~~~

For further information, see the [`stack exec` command](exec_command.md)
documentation.
