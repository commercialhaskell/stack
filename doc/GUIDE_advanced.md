<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# User guide (advanced)

Some of Stack's features will not be needed regularly or by all users. This part
of the guide provides information about those features, organised as a reference
guide. Some of the features are complex and separate pages are dedicated to
them.

## Environment variables

The existence or content of certain environment variables can affect how Stack
behaves. For further information, see the
[environment variables](environment_variables.md) documentation.

## YAML configuration files

Stack is configured by the content of YAML files. A global YAML configuration
file contains non-project specific options. A project-level YAML configuration
file contains project-specific options and may contain non-project specific
options. For further information, see the
[YAML configuration](yaml_configuration.md) documentation.

## Global flags and options

Stack can also be configured by flags and options on the command line. Global
flags and options apply to all of Stack's commands. For further information, see
the [global flags and options](global_flags.md) documentation.

## Stack commands

Stack's commands are listed below, in alphabetical order.

* [`bench`](build_command.md) - a synonym for `stack build --bench`
* [`build`](build_command.md) - build packages
* [`clean`](clean_command.md) - delete build artefacts for the project packages
* [`config`](config_command.md) - access and modify Stack's configuration
* [`docker`](docker_command.md) - use Stack with Docker
* [`dot`](dot_command.md) - dependency visualization
* [`eval`](eval_command.md) - evaluate some Haskell code inline
* [`exec`](exec_command.md) - executate a command in the Stack environment
* [`haddock`](build_command.md) - a synonym for `stack build --haddock`
* [`hoogle`](hoogle_command.md) - run `hoogle`
* [`hpc`](hpc_command.md) - generate Haskell Program Coverage (HPC) code coverage
  reports
* [`ghc`](ghc_command.md) - run `ghc`
* [`ghci`](ghci.md) - run GHCi, a REPL environment
* [`ide`](ide_command.md) - information for an integrated development
  environment (IDE)
* [`init`](init_command.md) - initialise Stack's project-level YAML configuration file for an
  existing project
* [`install`](build_command.md) - a synonym for `stack build --copy-bins`
* [`list`](list_command.md) - list packages on Hackage or in a snapshot
* [`ls`](ls_command.md) - list information about Stack
* [`new`](new_command.md) - create a new project with Stack
* [`path`](path_command.md) - information about locations used by Stack
* [`purge`](purge_command.md) - delete the Stack working directories
* [`query`](query_command.md) - information about the build
* [`repl`](ghci.md) - a synonym for `stack ghci`
* [`run`](run_command.md) - build and run an executable
* [`runghc`](runghc_command.md) - run `runghc`
* [`runhaskell`](runghc_command.md) - a synonym for `stack runghc`
* [`script`](script_command.md) - run a Haskell source file as a script
* [`sdist`](sdist_command.md) - create an archive file for a package, in a form
  accepted by Hackage
* [`setup`](setup_command.md) - get GHC for a Stack project
* [`templates`](templates_command.md) - information about templates for use with
  `stack new`
* [`test`](build_command.md) - a synonym for `stack build --test`
* [`uninstall`](uninstall_command.md) - information about how to uninstall Stack
* [`unpack`](unpack_command.md) - unpack one or more packages locally
* [`update`](update_command.md) - update the package index
* [`upgrade`](upgrade_command.md) - upgrade Stack
* [`upload`](upload_command.md) - upload a package to Hackage
