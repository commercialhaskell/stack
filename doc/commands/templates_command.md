<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack templates` command

~~~text
stack templates
~~~

`stack templates` provides information to the standard output stream about
project templates used with the [`stack new` command](new_command.md).

Project templates are specified in `.hsfiles` files. The format of those files
is documented at the
[`commercialhaskell/stack-templates`](https://github.com/commercialhaskell/stack-templates#project-template-format)
repository on GitHub.

Any GitHub, GitLab, Bitbucket or Codeberg repository named `stack-templates`
can provide project template files. For example, a template file
`username/stack-templates/my-template.hsfiles` on GitHub can be identified as
`username/my-template` when using `stack new`. The relevant service can be
specified by a prefix: `github:` for [GitHub](https://github.com/) (the default
service), `gitlab:` for [GitLab](https://gitlab.com), `bitbucket:` for
[Bitbucket](https://bitbucket.com), or `codeberg:` for
[Codeberg](https://codeberg.org).

[`commercialhaskell/stack-templates`](https://github.com/commercialhaskell/stack-templates#project-template-format)
on GitHub is the default repository for project templates. Its username
(`commercialhaskell`) does not need to be specified when using `stack new`.

The project template that `stack new` uses by default is named `new-template`
and provided at the default repository.

The default repository provides 24 other project templates. Its Wiki provides
a description of some of those templates and information about the location of
other templates.
