<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack new` command

~~~text
stack new PACKAGE_NAME [--bare] [TEMPLATE_NAME] [-p|--param KEY:VALUE] [DIR(S)]
          [--omit-packages] [--force] [--ignore-subdirs]
~~~

`stack new` creates a new Stack project for a package using a template.

The project is created in a new directory named after the package, unless the
`--bare` flag is passed, in which case the project is created in the current
directory.

The template used is a default one (named `new-template`), unless another
template is specified as an argument.

The `--param <key>:<value>` option specifies a key-value pair to populate a key
in a template. The option can be specified multiple times.

The arguments specifying directories and the `--ignore-subdirs`, `--force` and
`--omit-packages` flags are as for the [`stack init` command](init_command.md).
