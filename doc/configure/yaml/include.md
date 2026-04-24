<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `!include` directive

Stack's configuration files are in the [YAML](https://yaml.org/) format. Stack
also supports the use of an `!include` local tag together with scalar content
that represents an absolute or relative path to another file. This provides a
directive that allows the content of one YAML file to be included in another.

The directive can be used in both
[project-level and global](index.md#project-level-and-global-configuration-files)
configuration files.

!!! note

    An included relative file path is relative to the directory containing the
    file with the `!include` directive.

!!! warning

    The [`stack config set`](../../commands/config_command.md#the-stack-config-set-commands)
    commands cannot modify a configuration file that excludes the relevant key
    and uses `!include` directives.

## Including a value

A value for a key can be provided by an included file. For example, given a file
`snapshot.yaml` in the project directory with the content:

~~~yaml
lts-24.37
~~~

the following project-level configuration file would use `lts-24.37` as the
snapshot:

~~~yaml
snapshot: !include snapshot.yaml
~~~

The included file replaces the `!include` directive with its content, so this is
equivalent to:

~~~yaml
snapshot: lts-24.37
~~~

## Including a sequence

The value provided by an included file is not limited to scalar content. It can
be a YAML sequence. For example, given a file `extra-deps.yaml` in the project
directory with the content:

~~~yaml
- acme-missiles-0.3
- text-short-0.1.6
~~~

the following project-level configuration file would use those as extra-deps:

~~~yaml
snapshot: lts-24.37
extra-deps: !include extra-deps.yaml
~~~

## Merging mappings

YAML's merge key (`<<`) is used to indicate that all of the keys of one or more
specified mappings should be inserted into the current mapping.

YAML's merge key can be combined with an `!include` directive to merge the
content of an included file into the current mapping. For example, given a file
`shared-config.yaml` in the project directory with the content:

~~~yaml
ghc-options:
  "$everything": -Wall
flags:
  my-package:
    my-flag: true
~~~

the following project-level configuration file would merge those options:

~~~yaml
snapshot: lts-24.37
<<: !include shared-config.yaml
~~~

This is equivalent to:

~~~yaml
snapshot: lts-24.37
ghc-options:
  "$everything": -Wall
flags:
  my-package:
    my-flag: true
~~~

The `!include` directive can also be placed on the line after the merge key:

~~~yaml
snapshot: lts-24.37
<<:
  !include shared-config.yaml
~~~

## Nested includes

Included files can themselves contain `!include` directives, allowing for nested
composition of configuration.

!!! note

    A file cannot include itself or a file that has already included the file.
    Stack detects and raises an error for cyclic includes.
