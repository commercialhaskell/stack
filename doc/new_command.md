<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack new` command

~~~text
stack new PACKAGE_NAME [--bare] [TEMPLATE_NAME] [-p|--param KEY:VALUE] [DIR(S)]
          [--omit-packages] [--force] [--ignore-subdirs]
~~~

`stack new` creates a new Stack project for a package using a project template.

A package name acceptable to Cabal comprises an alphanumeric 'word'; or two or
more such words, with the words separated by a dash character (`-`). A word
cannot be comprised only of the characters `0` to `9`. An alphanumerical
character is one in one of the Unicode categories Lu (Letter, uppercase),
Ll (Letter, lowercase), Lt (Letter, titlecase), Lm (Letter, modifier),
Lo (Letter, other), Nd (Number, decimal digit), Nl (Number, letter), and
No (Number, other).

!!! note

    In the case of Hackage and acceptable package names, an alphanumerical
    character is limited to one of `A` to `Z`, `a` to `z`, and `0` to `9`.

The project is created in a new directory named after the package, unless the
`--bare` flag is passed, in which case the project is created in the current
directory.

!!! note

    The name of a project is not constrained to be an acceptable package name. A
    single-package project can be renamed to differ from the name of its
    package.

The `--param <key>:<value>` option specifies a key-value pair to populate a key
in a template. The option can be specified multiple times.

The arguments specifying directories and the `--ignore-subdirs`, `--force` and
`--omit-packages` flags are as for the [`stack init` command](init_command.md).

## Project templates

A project template file can be located in a repository named `stack-templates`
on GitHub, GitLab or Bitbucket; at a URL; or on the local file system.

Project template file names have the extension `.hsfiles`. The extension does
not need to be specified with `stack new`.

A project template file `my-template.hsfiles` in a repository
`username/stack-templates` on GitHub, GitLab or Bitbucket can be specified
with `stack new` as:

~~~test
<service>:username/my-template
~~~

where `<service>` is one of `github` for [GitHub](https://github.com/),
`gitlab` for [GitLab](https://gitlab.com), or `bitbucket` for
[Bitbucket](https://bitbucket.com).

The default service is GitHub, the default username is `commercialhaskell` and
the default project template name is `new-template`.

## Examples

Create a project for package `my-project` in new directory `my-project` with the
default project template file:

~~~text
stack new my-project
~~~

Create a project for package `my-package` in the current directory with the
default project template file:

~~~text
stack new my-package --bare
~~~

Create a project with the `rio` project template at the default repository:

~~~text
stack new my-project rio
~~~

Create a project with the `mysql` project template provided by the
`yesodweb/stack-templates` repository on GitHub:

~~~text
stack new my-project yesodweb/mysql
~~~

Create a project with the `my-template` project template provided by the
`username/stack-templates` repository on Bitbucket:

~~~text
stack new my-project bitbucket:username/my-template
~~~

Create a project with the `my-template.hsfiles` project template file at
`https://example.com`:

~~~text
stack new my-project https://example.com/my-template
~~~

Create a project with the local project template file
`<path_to_template>/my-template.hsfiles`:

~~~text
stack new my-project <path_to_template_file>/my-template
~~~
