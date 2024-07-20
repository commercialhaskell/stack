  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 9. Locations used by Stack

Generally, you don't need to worry about where Stack stores various files. But
some people like to know this stuff. That's when the `stack path` command is
useful.

## The `stack path` command

`stack path --help` explains the available options and, consequently,
the output of the command:

~~~text
--stack-root             Global Stack root directory
--global-config          Global Stack configuration file
--project-root           Project root (derived from stack.yaml file)
--config-location        Configuration location (where the stack.yaml file is)
--bin-path               PATH environment variable
--programs               Install location for GHC and other core tools (see
                         'stack ls tools' command)
--compiler-exe           Compiler binary (e.g. ghc)
--compiler-bin           Directory containing the compiler binary (e.g. ghc)
--compiler-tools-bin     Directory containing binaries specific to a
                         particular compiler
--local-bin              Directory where Stack installs executables (e.g.
                         ~/.local/bin (Unix-like OSs) or %APPDATA%\local\bin
                         (Windows))
--extra-include-dirs     Extra include directories
--extra-library-dirs     Extra library directories
--snapshot-pkg-db        Snapshot package database
--local-pkg-db           Local project package database
--global-pkg-db          Global package database
--ghc-package-path       GHC_PACKAGE_PATH environment variable
--snapshot-install-root  Snapshot installation root
--local-install-root     Local project installation root
--snapshot-doc-root      Snapshot documentation root
--local-doc-root         Local project documentation root
--local-hoogle-root      Local project documentation root
--dist-dir               Dist work directory, relative to package directory
--local-hpc-root         Where HPC reports and tix files are stored
~~~

In addition, `stack path` accepts the flags above on the command line to state
which keys you're interested in. This can be convenient for scripting. As a
simple example, let's find out the sandboxed versions of GHC that Stack
installed:

=== "Unix-like"

    Command:

    ~~~text
    ls $(stack path --programs)/*.installed
    /home/<user_name>/.stack/programs/x86_64-linux/ghc-9.0.2.installed
    ~~~

=== "Windows (with PowerShell)"

    Command:

    ~~~text
    dir "$(stack path --programs)/*.installed"

    Directory: C:\Users\mikep\AppData\Local\Programs\stack\x86_64-windows

    Mode                 LastWriteTime         Length Name
    ----                 -------------         ------ ----
    -a---          27/07/2022  5:40 PM              9 ghc-9.0.2.installed
    -a---          25/02/2022 11:39 PM              9 msys2-20210604.installed
    ~~~

While we're talking about paths, to wipe our Stack install completely, here's
what typically needs to be removed:

1. the Stack root folder (see `stack path --stack-root`, before you uninstall);
2. if different, the folder containing Stack's global YAML configuration file
   (see `stack path --global-config`, before you uninstall);
3. on Windows, the folder containing Stack's tools (see `stack path --programs`,
   before you uninstall), which is located outside of the Stack root folder; and
4. the `stack` executable file (see `which stack`, on Unix-like operating
   systems, or `where.exe stack`, on Windows).

You may also want to delete `.stack-work` folders in any Haskell projects that
you have built using Stack. The `stack uninstall` command provides information
about how to uninstall Stack.
