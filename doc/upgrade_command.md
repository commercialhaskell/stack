<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack upgrade` command

Either:

~~~text
stack upgrade [--binary-only] [--binary-platform ARG] [--force-download]
              [--binary-version ARG] [--github-org ARG] [--github-repo ARG]
~~~

or:

~~~text
stack upgrade [--source-only] [--git] [--git-repo ARG] [--git-branch ARG]
~~~

`stack upgrade` will get a new version of Stack, either from an existing
binary distribution (pass the `--binary-only` flag, the default) or from
compiling source code (pass the `--source-only` flag). The `--binary-only` and
`--source-only` flags are alternatives.

When compiling from source code, by default:

*   Stack will obtain the source code for the most recent version in the package
    index (eg Hackage). Pass the flag `--git` to specify the most recent version
    from the `master` branch of Stack's repository.
