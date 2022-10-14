<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Developing on Windows #

On Windows, Stack comes with an installation of [MSYS2](https://www.msys2.org/).
MSYS2 will be used by Stack to provide a Unix-like shell and environment for
Stack. This may be necessary for installing some Haskell packages, such as those
which use `configure` scripts, or if your project needs some additional tools
during the build phase.

No matter which terminal software you choose (Windows Terminal, Console Windows
Host, Command Prompt, PowerShell, Git bash or any other) you can use this
environment too by executing all programs through
`stack exec -- <program_name>`.

Executables and libraries can be installed with the MSYS2 package manager
`pacman`. All tools can be found in the
[package list](https://github.com/msys2/msys2/wiki/Packages). A [list of
commands](https://github.com/msys2/msys2/wiki/Using-packages) that work with
`pacman` is also available. Just remember that `pacman` &mdash; like all other
tools &mdash; should be started with `stack exec -- pacman`.

Command `stack path --bin-path` to see the PATH in the Stack environment. On
Windows, it includes the `\mingw64\bin`, `\usr\bin` and `\usr\local\bin`
directories of the Stack-supplied MSYS2. If your executable depends on files
(for example, dynamic-link libraries) in those directories and you want ro run
it outside of the Stack environment, you will need to ensure copies of those
files are on the PATH.

The Stack-supplied MSYS2 can itself be updated with the Stack-supplied `pacman`.
See the MSYS2 guide
['III. Updating packages'](https://www.msys2.org/wiki/MSYS2-installation/). If
the Stack-supplied `pacman` has a version that is 5.0.1.6403 or greater (see
`stack exec -- pacman --version`) then the command to update is simply:

    stack exec -- pacman -Syuu

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

* For [text-icu](https://github.com/bos/text-icu) install
  `mingw64/mingw-w64-x86_64-icu`

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
