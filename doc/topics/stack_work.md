<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack work directories

Stack work directories are directories within a local project or package
directory in which Stack stores files created during the build process. Stack
can be used without an understanding of the content of those directories. In
particular, the [`stack exec`](../commands/exec_command.md) command sets up an
environment where relevant subdirectories of the project Stack work directory
are on the PATH.

## Naming

By default, Stack work directories are named `.stack-work`. The name can be
overidden by:

* the use of the
  [`STACK_WORK` environment variable](../configure/environment_variables.md#stack_work);
* the [`work-dir`](../configure/yaml/non-project.md#work-dir) non-project
  specific configuration option; or
* the [`--work-dir`](../configure/global_flags.md#-work-dir-option) command line
  option.

Given the location of Stack work directories, the name of the work directories
must be a relative path to a directory.

## Location

If the work directory does not already exist, it will be created by the
[`stack build`](../commands/build_command.md) command as a subdirectory of each
project package directory and, if different, the project directory.

## Project package Stack work directory

The Stack work directory for a project package will contain a `dist` directory.
This directory will contain a path to a directory containing:

* a `build` directory;
* a `package.conf.inplace` directory;
* a `stack-build-caches` directory;
* a `build-lock` file;
* a `setup-config` file;
* a `stack-cabal-mod` file. This file is used by Stack only for its modification
  time;
* a `stack-project-root` file. This file contains an absolute path to the
  project root directory; and
* a `stack-setup-config-mod` file. This file is used by Stack only for its
  modification time.

The directory, relative to the project package directory or the project
directory, is the one reported by
[`stack path --dist-dir`](../commands/path_command.md).

=== "Unix-like"

    On Unix-like operating systems, the path to the directory is a directory
    named after the platform (including Stack's classification of variants of
    Linux distributions) followed by a directory named after the GHC version.

=== "Windows"

    On Windows, the path to the directory is an eight-character hash of the
    path that applies on Unix-like operating systems.

## Project Stack work directory

The Stack work directory for a project will contain a `install` directory.
This directory will contain a path to a directory containing:

* a `bin` directory, containing built executable files;
* a `doc` directory, containing a directory for each project package. This is
  the directory reported by
  [`stack path --local-doc-root`](../commands/path_command.md);
* if the [`stack hpc`](hpc_command.md) command is used, a `hpc` directory. This
  is the directory reported by
  [`stack path --local-hpc-root`](../commands/path_command.md);
* a `lib` directory, containing a directory named after the platform and the
  GHC version and, within that, a directory for each project package;
* a `pkgdb` directory. This is the directory reported by
  [`stack path --local-pkg-db`](../commands/path_command.md);
* a `stack.sqlite3` file; and
* a `stack.sqlite3.pantry-write-lock` file.

The directory is the one reported by
[`stack path --local-install-root`](../commands/path_command.md).

=== "Unix-like"

    On Unix-like operating systems, the path to the directory is a directory
    named after the platform (including Stack's classification of variants of
    Linux distributions) followed by a directory named after a SHA 256 hash
    (see further below) followed by a directory named after the version number
    of GHC.

    The SHA 256 hash is a hash of the following information:

    * the path to the specified compiler;
    * the information about the compiler provided by `ghc --info`;
    * the options that Stack passes to GHC for package that is not a project
      package; and
    * information about the immutable dependencies: their location, whether or
      not Haddock documentation is to be built, their flags, their GHC options,
      and their Cabal configuration options.

    The options that Stack passes to GHC for a package that is not a project
    package depend on:

    * the specification of
      [profiling](https://docs.haskellstack.org/en/stable/build_command/#flags-affecting-ghcs-behaviour);
    * the specification of
      [stripping](https://docs.haskellstack.org/en/stable/build_command/#flags-affecting-ghcs-behaviour); and
    * if
      [`apply-ghc-options: everything`](../configure/yaml/non-project.md#apply-ghc-options)
      is specified, any GHC command line options specified on the command line.

    !!! note

        As a consequence, the path reported by the following commands will
        differ (and similarly for the paths established by the
        [`stack exec`](../commands/exec_command.md) command):

        ~~~text
        stack path --local-install-root
        stack --profile path --local-install-root
        stack --no-strip path --local-install-root
        stack --profile --no-strip path --local-install-root
        ~~~

=== "Windows"

    On Windows, the path to the directory is an eight-character hash of the
    path that applies on Unix-like operating systems.

Following a `stack ghci` or `stack repl` command, the Stack work directory for
a project will contain a `ghci` directory. This directory will contain paths to
`cabal_macos.h` files that are generated automatically by Cabal.

!!! note

    Haskell Language Server makes use of the `stack ghci` command to obtain
    information.

If the [`stack hoogle`](../commands/hoogle_command.md) command is used, the
Stack work directory for a project will contain a `hoogle` directory. This
directory will contain a directory being the one reported by
[`stack path --local-hoogle-root`](../commands/path_command.md). The naming of
the path to the directory is same as for the path to the directory in the
`install` directory.
