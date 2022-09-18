<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Shell auto-completion

The following adds support for the tab completion of standard Stack arguments to
the following shell programs: Bash, Zsh (the Z shell) and fish. Completion of
file names and executables within Stack is still lacking. For further
information, see issue
[#823](https://github.com/commercialhaskell/stack/issues/832).

!!! info

    Stack's completion library provides
    [hidden options](https://github.com/pcapriotti/optparse-applicative#bash-zsh-and-fish-completions)
    for Bash, Zsh, and fish which output commands used for shell
    auto-completion. For example:

    ~~~bash
    $ stack --bash-completion-script stack
    _stack()
    {
        local CMDLINE
        local IFS=$'\n'
        CMDLINE=(--bash-completion-index $COMP_CWORD)

        for arg in ${COMP_WORDS[@]}; do
            CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
        done

        COMPREPLY=( $(stack "${CMDLINE[@]}") )
    }

    complete -o filenames -F _stack stack
    ~~~

=== "Bash"

    Add the output of the following command to your preferred completions file
    (e.g. `~/.config/bash_completions.d/stack`).

    ~~~bash
    stack --bash-completion-script $(which stack)
    ~~~

    You may need to `source` this.

=== "Zsh"

    Add the output of the following command to your preferred completions file
    (e.g. `~/.config/zsh/completions/_stack`).

    ~~~zsh
    stack --zsh-completion-script $(which stack)
    ~~~

    You won't need to `source` this, but do update your `fpath`:

    ~~~zsh
    fpath=($HOME/.config/zsh/completions $fpath)
    autoload -U compinit && compinit
    ~~~

=== "fish"

    Add the output of the following command to your preferred completions file
    (e.g. `~/.config/fish/completions/stack.fish`).

    ~~~fish
    stack --fish-completion-script $(which stack)
    ~~~
