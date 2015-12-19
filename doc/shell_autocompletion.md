# Shell Auto-completion

Note: if you installed a package for you Linux distribution, the bash completion
file was automatically installed (you may need the `bash-completion` package to
have it take effect).

The following adds support for shell tab completion for standard Stack arguments, although completion for filenames and executables etc. within stack is still lacking (see [issue 823](https://github.com/commercialhaskell/stack/issues/832)).

## for bash users

you need to run following command
```
eval "$(stack --bash-completion-script stack)"
```
You can also add it to your `.bashrc` file if you want.

## for ZSH users

Copy the `_stack` and `_completion_helpers` files into a directory on your `$fpath`, e.g. ~/.oh-my-zsh/custom/completions/.


