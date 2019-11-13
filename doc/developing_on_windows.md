<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Developing on windows #

On Windows, Stack comes with an installation of
[msys2](https://www.msys2.org/). Msys2 will be used by Stack to
provide a unix-like shell for Stack. This may be necessary for installing some Haskell packages, such as those which use `configure` scripts.
No
matter which terminal you choose (cmd.exe, powershell, git bash or any
other) you can use this environment too by executing all programs
through `stack exec -- program`. This is especially useful if your
project needs some additional tools during the build phase.

Executables and libraries can be installed with Pacman. All tools can
be found [in the package
list](https://github.com/msys2/msys2/wiki/Packages). A [list of
commands](https://github.com/msys2/msys2/wiki/Using-packages) that
work with Pacman is also available. Just remember that
pacman&mdash;like all other tools&mdash;should be started with `stack
exec -- pacman`.

## Setup.hs ##

`Setup.hs` is automatically run inside the stack environment. So when
you need to launch another tool you don't need to prefix the command
with `stack exec --` within the custom `Setup.hs` file.

## Pacman packages to install for common Haskell packages ##

The following lists Pacman packages known to allow the installation of
some common Haskell packages on Windows. Feel free to submit
additional entries via a pull request.

* For [text-icu](https://github.com/bos/text-icu) install `mingw64/mingw-w64-x86_64-icu`

## Cmake ##

Cmake has trouble finding other tools even if they are available on
the `PATH`. Likely this is not a cmake problem but one of the
environment not fully integrating. For example GHC comes with a copy
of GCC which is not installed by msys itself. If you want to use this
GCC you can provide a full path to it, or find it first with
`System.Directory.findExecutable` if you want to launch GCC from a
Haskell file such as `Setup.hs`.

Experience tells that the `mingw-w64` versions of make and cmake are
most likely to work. Though there are other versions available through
pacman, so have a look to see what works for you. Both tools can be
installed with the commands:

```
stack exec -- pacman -S mingw-w64-x86_64-make
stack exec -- pacman -S mingw-w64-x86_64-cmake
```

Even though make and cmake are then both installed into the same
environment. Cmake still seems to have trouble to find make. To help
cmake find GCC and make supply the following flags:

```
-DCMAKE_C_COMPILER=path
-DCMAKE_MAKE_PROGRAM=path
```
