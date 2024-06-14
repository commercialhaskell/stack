<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Glossary

The following terms are used in Stack's documentation.

|Term               |Meaning                                                   |
|-------------------|----------------------------------------------------------|
|Cabal              |The Haskell Common Architecture for Building Applications and Libraries, provided by the [`Cabal` package](https://hackage.haskell.org/package/Cabal). Also referred to as Cabal (the library) to distinguish it from Cabal (the tool).|
|Cabal file|A file containing a [package description](https://cabal.readthedocs.io/en/stable/cabal-package.html) used by Cabal, named `<package_name>.cabal`.|
|Cabal (the tool)|The Haskell tool used for building provided by the [`cabal-install` package](https://hackage.haskell.org/package/cabal-install).|
|CI                 |Continuous integration.                                   |
|CMake              |A [system](https://cmake.org/) for managing build processes.|
|`config.yaml`      |A global and non-project-specific configuration file used by Stack.|
|dependency         |A Haskell package other than a project package and on which a project package depends (directly or indirectly), located locally or elsewhere.|
|Docker             |A [platform](https://www.docker.com/) for developing,  shipping, and running applications. It can package and run an application in a loosely isolated environment called a _container_.|
|Emacs              |[GNU Emacs](https://www.gnu.org/software/emacs/), an extensible, customisable text editor.|
|extra-deps         |Extra dependencies (one version of each) that add to, or shadow, those specified in a snapshot.|
|FreeBSD            |A Unix-like operating system.                             |
|GCC                |The [GNU Compiler Collection](https://gcc.gnu.org/) or its executable `gcc`.|
|GHC                |The [Glasgow Haskell Compiler](https://www.haskell.org/ghc/).|
|GHC boot package   |A package that comes with GHC, is included in GHC's global package database, and is not included in a Stackage snapshot. See the output of command `stack exec -- ghc-pkg list --global`.|
|GHCi               |GHC's [interactive environment](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html).|
|GHCJS              |A Haskell to JavaScript [compiler](https://github.com/ghcjs/ghcjs).|
|GHCup              |An [installer](https://www.haskell.org/ghcup/) for Haskell.
|Git                |A [distributed version control system](https://git-scm.com/).|
|GPG                |The [GNU Privacy Guard](https://gnupg.org/) or GnuPG, software that allows you to encrypt or sign your data and communications.|
|Hackage            |The [Haskell Package Repository](https://hackage.haskell.org/).
|Haddock            |The [document generation tool](https://hackage.haskell.org/package/haddock) for Haskell libraries.|
|'Haskell' extension|The ['Haskell' extension]() for VS Code.                  |
|HLS                |[Haskell Language Server](https://haskell-language-server.readthedocs.io/en/latest/), an implementation of the Language Server Protocol for Haskell.|
|Homebrew           |A [package manager](https://brew.sh/) for macOS or Linux, or its executable `brew`.|
|Hoogle             |A Haskell API [search engine](https://hoogle.haskell.org/).|
|Hpack              |A [format](https://github.com/sol/hpack) for Haskell packages or the executable `hpack` that produces a Cabal file from `package.yaml`.|
|Linux              |A family of operating systems based on the Linux kernel.  |
|macOS              |The primary operating system for Apple's Mac computers. Previously known as Mac OS X or OS X.|
|Make               |A [build automation tool](https://www.gnu.org/software/make/).|
|MSYS2              |The [MSYS2](https://www.msys2.org/) software distribution and building platform for Windows.|
|Nix                |A purely functional [package manager](https://nixos.org/), available for Linux and macOS.|
|package            |A Haskell package is an organised collection of Haskell code and related files. It is described by a Cabal file or a `package.yaml` file, which is itself part of the package.|
|`package.yaml`     |A file that describes a package in the Hpack format.      |
|Pantry             |A library for content-addressable Haskell package management, provided by the [`pantry` package](https://hackage.haskell.org/package/pantry). A dependency of Stack.|
|PATH               |The `PATH` environment variable, specifying a list of directories searched for executable files.|
|project            |A Stack project is a local directory that contains a project-level configuration file (`stack.yaml`, by default). A project may relate to more than one project package.|
|project package    |A Haskell package that is part of a project and located locally. Distinct from a dependency located locally.|
|PVP                |The Haskell [Package Versioning Policy](https://pvp.haskell.org/), which tells developers of libraries how to set their version numbers.|
|REPL               |An interactive (run-eval-print loop) programming environment.|
|resolver           |A synonym for snapshot.                                   |
|`Setup.hs`         |A project-specific file used by Cabal to perform setup tasks.|
|snapshot           |A snapshot defines a GHC version, a set of packages (one version of each), and build flags or other settings.|
|Stack              |The Haskell Tool Stack project or its executable `stack`. |
|`stack.yaml`       |A project-level configuration file used by Stack, which may also contain non-project-specific options.|
|Stackage           |A [distribution](https://www.stackage.org/) of compatible Haskell packages.|
|Stack root         |A directory in which Stack stores important files. See `stack path --stack-root`. On Windows, or if Stack is configured to use the XDG Base Directory Specification, Stack also stores important files outside of the Stack root.|
|Stack work directory|A directory within a local project or package directory in which Stack stores files created during the build process. Named `.stack-work`, by default.|
|Unix-like operating systems|Linux, FreeBSD and macOS.                         |
|VS Code            |[Visual Studio Code](https://code.visualstudio.com/), a source code editor.|
|Windows            |A group of operating systems developed by Microsoft.      |
|WSL                |[Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/). Provides a Linux environment on Windows.|
|XDG Base Directory Specification|A [specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) of directories relative to which files should be located.|
|YAML               |A human-friendly [data serialization language](https://yaml.org/).|
