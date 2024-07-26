<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Developing on Windows #

On Windows, Stack comes with an installation of [MSYS2](https://www.msys2.org/).
An environment of MSYS2 (by default, `MINGW64` on 64-bit Windows or `MINGW32` on
32-bit Windows) will be used by Stack to provide a Unix-like shell and
environment for Stack. This may be necessary for installing some Haskell
packages, such as those which use `configure` scripts, or if your project needs
some additional tools during the build phase.

No matter which terminal software you choose (Windows Terminal, Console Windows
Host, Command Prompt, PowerShell, Git bash or any other) you can use this
environment too by executing all programs through
`stack exec -- <program_name>`.

Executables and libraries can be installed with the MSYS2 package manager
`pacman`. All tools can be found in the [index](https://packages.msys2.org) to
MSYS2 packages. A [guide](https://www.msys2.org/docs/package-management/) to
package management with `pacman` is also available. `pacman` &mdash; like all
other tools in the Stack environment &mdash; should be started with
`stack exec -- pacman`. Help about `pacman` commands (operations) can be
obtained by `stack exec -- pacman --help`. Help about a specific `pacman`
operation can be obtained by using `--help` (or `-h`) with an operation. For
example, help about the operation `--sync` (or `-S`) can be obtained with
`stack exec -- pacman --sync --help` or, equivalently,
`stack exec -- pacman -Sh`.

Command `stack path --bin-path` to see the PATH in the Stack environment. If the
relevant MSYS2 environment is `MINGW64`, on Windows, it includes the
`\mingw64\bin`, `\usr\bin` and `\usr\local\bin` directories of the
Stack-supplied MSYS2. (It includes the corresponding directory if the relevant
MSYS2 environment is other than `MINGW64`.) If your executable depends on files
(for example, dynamic-link libraries) in those directories and you want to run
it outside of the Stack environment, you will need to ensure copies of those
files are on the PATH.

Command `stack path --extra-include-dirs` and `stack path --extra-library-dirs`
to see the extra directories searched for C header files or system libraries
files in the Stack environment. If the relevant MSYS2 environment is `MINGW64`,
on Windows, it includes the `\mingw64\include` (include) and the `\mingw64\lib`
and `\mingw64\bin` directories (library) of the Stack-supplied MSYS2. (It
includes the corresponding directories if the relevant MSYS2 environment is
other than `MINGW64`.)

For further information about configuring the relevant MSYS2 environment, see
Stack's [`msys-environment`](../configure/yaml/non-project.md#msys-environment)
non-project specific configuration option documentation.

## Updating the Stack-supplied MSYS2 ##

The Stack-supplied MSYS2 can itself be updated with the Stack-supplied `pacman`.
See the MSYS2 guide [Updating MSYS2](https://www.msys2.org/docs/updating/). If
the Stack-supplied `pacman` has a version that is 5.0.1.6403 or greater (see
`stack exec -- pacman --version`) then the command to update is simply:

    stack exec -- pacman -Suy

This command may need to be run more than once, until everything is reported by
`pacman` as 'up to date' and 'nothing to do'.

## Setup.hs ##

`Setup.hs` is automatically run inside the Stack environment. So when you need
to launch another tool you don't need to prefix the command with `stack exec --`
within the custom `Setup.hs` file.

## Pacman packages to install for common Haskell packages ##

The following lists MSYS2 packages known to allow the installation of some
common Haskell packages on Windows. Feel free to submit additional entries via a
pull request.

*   For [text-icu](https://hackage.haskell.org/package/text-icu) install
    `mingw64/mingw-w64-x86_64-icu`.

*   For [zlib >= 0.7](https://hackage.haskell.org/package/zlib) the default
    Cabal flag `pkg-config` is `true` and requires executable `pkg-config` on
    the PATH. MSYS2 [defaults](https://www.msys2.org/docs/pkgconfig/) to
    [`pkgconf`](https://packages.msys2.org/package/pkgconf?repo=msys&variant=x86_64)
    as its `pkg-config` implementation. Installation:

        stack exec -- pacman -S pkgconf

    Alternatively, build with `--flag zlib:-pkg-config`.

## CMake ##

CMake has trouble finding other tools even if they are available on the PATH.
Likely this is not a CMake problem but one of the environment not fully
integrating. For example GHC comes with a copy of GCC which is not installed by
MSYS2 itself. If you want to use this GCC you can provide a full path to it, or
find it first with `System.Directory.findExecutable` if you want to launch GCC
from a Haskell file such as `Setup.hs`.

Experience tells that the `mingw-w64` versions of Make and CMake are most
likely to work. Though there are other versions available through `pacman`, so
have a look to see what works for you. Both tools can be installed with the
commands:

    stack exec -- pacman -S mingw-w64-x86_64-make
    stack exec -- pacman -S mingw-w64-x86_64-cmake

Even though Make and CMake are then both installed into the same environment,
CMake still seems to have trouble to find Make. To help CMake find GCC and Make
supply the following flags:

    -DCMAKE_C_COMPILER=path
    -DCMAKE_MAKE_PROGRAM=path
