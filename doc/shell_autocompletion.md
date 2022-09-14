<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Shell auto-completion

The following adds support for the tab completion of standard Stack arguments to
some of the more popular shell programs: Bash, zsh, and fish. Completion of file
names and executables within Stack is still lacking. For further information,
see issue [#823](https://github.com/commercialhaskell/stack/issues/832).

=== "Bash"

    Issue the following command or add it to your `~/.bashrc` file:

    ~~~bash
    eval "$(stack --bash-completion-script stack)"
    ~~~

    !!! info

        Stack's hidden option `--bash-completion-script <stack_executable_name>`
        outputs a command that can be evaluated by Bash. For example:

        ~~~text
        stack --bash-completion-script stack
        _stack.exe()
        {
            local CMDLINE
            local IFS=$'\n'
            CMDLINE=(--bash-completion-index $COMP_CWORD)

            for arg in ${COMP_WORDS[@]}; do
                CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
            done

            COMPREPLY=( $(stack "${CMDLINE[@]}") )
        }
        ~~~

=== "Zsh"

    The Zsh
    [manual](https://zsh.sourceforge.io/Doc/Release/Completion-System.html#Completion-System)
    explains:

    > The function `bashcompinit` provides compatibility with bash’s
    programmable completion system. When run it will define the functions,
    `compgen` and `complete` which correspond to the bash builtins with the same
    names. It will then be possible to use completion specifications and
    functions written for bash.

    Consequently, you must:

    1.  launch `compinint`
    2.  launch `bashcompinit`
    3.  eval Stack's Bash completion script

    Issue the following commands or that them to your `~/.zshrc` file:

    ~~~zsh
    autoload -U +X compinit
    compinit
    autoload -U +X bashcompinit
    bashcompinit
    eval "$(stack --bash-completion-script stack)"
    ~~~

    !!! info

        If you already have quite a large `.zshrc` file, or if you use
        `oh-my-zsh`, `compinit` will probably already be loaded.

=== "Fish"

    Add the output of the following command to your preferred completions file
    (e.g. `~/.config/fish/completions/stack.fish`).

    ~~~fish
    stack --fish-completion-script stack
    ~~~
