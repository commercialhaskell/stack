  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 13. Using GHC interactively

GHCi is the interactive GHC environment, a.k.a. the REPL. You *could* access it
with command:

~~~text
stack exec ghci
~~~

But that will not load up locally written modules for access. For that, use the
`stack ghci` or `stack repl` commands, which are equivalent.

## The `stack ghci` or `stack repl` command

To then load
modules from your project in GHCi, use the `:module` command (`:m` for short)
followed by the module name.

!!! note

    If you have added packages to your project please make sure to mark them as
    extra-deps for faster and reliable usage of `stack ghci`. Otherwise GHCi may
    have trouble due to conflicts of compilation flags or having to
    unnecessarily interpret too many modules. See Stack's project-level
    [configuration](../configure/yaml/project.md#extra-deps) to learn how to
    configure a package as an extra-dep.

For further information, see the [REPL environment](../commands/ghci_command.md)
documentation.
