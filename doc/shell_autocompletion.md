# Shell Auto-completion

Note: if you installed a package for you Linux distribution, the bash
completion file was automatically installed (you may need the `bash-completion`
package to have it take effect).

The following adds support for shell tab completion for standard Stack
arguments, although completion for filenames and executables etc. within stack
is still lacking (see [issue
823](https://github.com/commercialhaskell/stack/issues/832)).

## for bash users

you need to run following command
```
eval "$(stack --bash-completion-script stack)"
```
You can also add it to your `.bashrc` file if you want.

## for ZSH users

documentation says:
> Zsh can handle bash completions functions. The latest development version of
> zsh has a function bashcompinit, that when run will allow zsh to read bash
> completion specifications and functions. This is documented in the zshcompsys
> man page. To use it all **you need to do is run bashcompinit at any time
> after compinit**. It will define complete and compgen functions corresponding
> to the bash builtins.

You must so:
  1. launch compinint
  2. launch bashcompinit
  3. eval stack bash completion script

```shell
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"
```

:information_source: If you already have quite a large zshrc, or if you use
oh-my-zsh, **compinit** will probably already be loaded. If you have a blank
zsh config, all of the 3 lines above are necessary.

:gem: tip: instead of running those 3 lines from your shell every time you want
to use stack, you can add those 3 lines in your $HOME/.zshrc file
