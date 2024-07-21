---
title: Commands
---
<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Commands (advanced)

Some of Stack's features will not be needed regularly or by all users. This part
of the guide and the part on [configuration](../configure/index.md) provide
information about somethose features, organised as a reference guide. Some of
the features are complex and separate pages are dedicated to them.

## Stack commands (thematic)

### Setting up

* [`setup`](setup_command.md) - get GHC for a Stack project (usually not needed)
* [`update`](update_command.md) - update the package index (usually not needed)
* [`new`](new_command.md) - create a new project with Stack
* [`init`](init_command.md) - initialise Stack's project-level YAML
  configuration file for an existing project

### Building

* [`build`](build_command.md) - build packages
* [`test`](build_command.md) - a synonym for `stack build --test`
* [`bench`](build_command.md) - a synonym for `stack build --bench`
* [`haddock`](build_command.md) - a synonym for `stack build --haddock`
* [`install`](build_command.md) - a synonym for `stack build --copy-bins`
* [`run`](run_command.md) - build and run an executable

### Docker-related

* [`docker`](docker_command.md) - use Stack with Docker

### Executing in the Stack environment

* [`exec`](exec_command.md) - executate a command in the Stack environment
* [`ghc`](ghc_command.md) - run `ghc`
* [`eval`](eval_command.md) - evaluate some Haskell code inline
* [`runghc`](runghc_command.md) - run `runghc`
* [`runhaskell`](runghc_command.md) - a synonym for `stack runghc`

### Using GHC interactively

* [`ghci`](ghci_command.md) - run GHCi, a REPL environment
* [`repl`](ghci_command.md) - a synonym for `stack ghci`

### Down/up loading local packages from/to Hackage

* [`unpack`](unpack_command.md) - unpack one or more packages locally
* [`sdist`](sdist_command.md) - create an archive file for a package, in a form
  accepted by Hackage
* [`upload`](upload_command.md) - upload a package to Hackage

### Cleaning-up

* [`clean`](clean_command.md) - delete build artefacts for the project packages
* [`purge`](purge_command.md) - delete the Stack working directories

### Amending Stack's configuration files

* [`config set`](config_command.md) - modify Stack's configuration

### Using Haskell code as a script
* [`script`](script_command.md) - run a Haskell source file as a script

### Getting information

* [`path`](path_command.md) - information about locations used by Stack
* [`ls`](ls_command.md) - list information about Stack
* [`list`](list_command.md) - list packages on Hackage or in a snapshot
* [`ide`](ide_command.md) - information for an integrated development
  environment (IDE)
* [`query`](query_command.md) - information about the build
* [`config env`](config_command.md) - modify Stack's configuration
* [`templates`](templates_command.md) - information about templates for use with
  `stack new`
* [`uninstall`](uninstall_command.md) - information about how to uninstall Stack

### Using tools in Stack's environment

* [`hoogle`](hoogle_command.md) - run `hoogle`
* [`dot`](dot_command.md) - dependency visualization
* [`hpc`](hpc_command.md) - generate Haskell Program Coverage (HPC) code
  coverage reports

### Managing Stack versions

* [`upgrade`](upgrade_command.md) - upgrade Stack

## Stack commands (alphabetical)

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
* [`hpc`](hpc_command.md) - generate Haskell Program Coverage (HPC) code
  coverage reports
* [`ghc`](ghc_command.md) - run `ghc`
* [`ghci`](ghci_command.md) - run GHCi, a REPL environment
* [`ide`](ide_command.md) - information for an integrated development
  environment (IDE)
* [`init`](init_command.md) - initialise Stack's project-level YAML
  configuration file for an existing project
* [`install`](build_command.md) - a synonym for `stack build --copy-bins`
* [`list`](list_command.md) - list packages on Hackage or in a snapshot
* [`ls`](ls_command.md) - list information about Stack
* [`new`](new_command.md) - create a new project with Stack
* [`path`](path_command.md) - information about locations used by Stack
* [`purge`](purge_command.md) - delete the Stack working directories
* [`query`](query_command.md) - information about the build
* [`repl`](ghci_command.md) - a synonym for `stack ghci`
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
