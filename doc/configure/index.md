---
title: Configure
---
<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Configuration (advanced)

Some of Stack's features will not be needed regularly or by all users. This part
of the guide and the part on Stack's [commands](../commands/index.md) provide
information about those features, organised as a reference guide. Some of the
features are complex and separate pages are dedicated to them.

The behaviour of Stack is configurable using environment variables, YAML
configuration files, global flags and options on the command line and
customisation scripts.

## Environment variables

The existence or content of certain environment variables can affect how Stack
behaves. For further information, see the
[environment variables](environment_variables.md) documentation.

## Configuration files

Stack is configured by the content of files in the YAML formal. A global
configuration file contains non-project specific options. A project-level
configuration file contains project-specific options and may contain non-project
specific options. For further information, see the
[configuration](yaml/index.md) documentation.

## Global flags and options

Stack can also be configured by flags and options on the command line. Global
flags and options apply to all of Stack's commands. For further information, see
the [global flags and options](global_flags.md) documentation.

## Customisation scripts

Stack's behaviour can also be affected by customisation scripts. For further
information, see the [customisation scripts](customisation_scripts.md)
documentation.
