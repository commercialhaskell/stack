<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `!include` directive

Stack's configuration files are in the [YAML](https://yaml.org/) format. Stack
supports a non-standard `!include` YAML directive that allows the content of
one YAML file to be included in another. The directive can be used in both
[project-level and global](index.md#project-level-and-global-configuration-files)
configuration files.

The included file path is relative to the directory containing the file with the
`!include` directive.

!!! warning

    The [`stack config set`](../../commands/config_command.md#the-stack-config-set-commands)
    commands cannot modify a configuration file that uses `!include` directives.

## Including a value

A value for a key can be provided by an included file. For example, given a file
`snapshot.yaml` with the content:

~~~yaml
lts-23.24
~~~

the following project-level configuration file would use `lts-23.24` as the
snapshot:

~~~yaml
snapshot: !include snapshot.yaml
packages:
- .
~~~

The included file replaces the `!include` directive with its content, so this is
equivalent to:

~~~yaml
snapshot: lts-23.24
packages:
- .
~~~

## Merging mappings

YAML's merge key (`<<`) can be combined with `!include` to merge the content of
an included file into the current mapping. For example, given a file
`shared-config.yaml` with the content:

~~~yaml
ghc-options:
  "$everything": -Wall
flags:
  my-package:
    dev: true
~~~

the following project-level configuration file would merge those options:

~~~yaml
snapshot: lts-23.24
<<: !include shared-config.yaml
packages:
- .
~~~

This is equivalent to:

~~~yaml
snapshot: lts-23.24
ghc-options:
  "$everything": -Wall
flags:
  my-package:
    dev: true
packages:
- .
~~~

The `!include` directive can also be placed on the line after the merge key:

~~~yaml
snapshot: lts-23.24
<<:
  !include shared-config.yaml
packages:
- .
~~~

## Including list items

The `!include` directive can also be used to include the contents of a file as a
list item. For example, given a file `extra-deps.yaml` with the content:

~~~yaml
- acme-missiles-0.3
- text-short-0.1.6
~~~

the following would use those as extra dependencies:

~~~yaml
snapshot: lts-23.24
extra-deps: !include extra-deps.yaml
~~~

## Nested includes

Included files can themselves contain `!include` directives, allowing for nested
composition of configuration. Stack detects and raises an error for cyclic
includes.

## Use with global configuration

The `!include` directive can also be used in the global configuration file
(`config.yaml`). For example, given a file `ghc-options.yaml` with the content:

~~~yaml
ghc-options:
  "$everything": -j4
~~~

the global configuration file could include it:

~~~yaml
<<: !include ghc-options.yaml
install-ghc: true
system-ghc: false
~~~
