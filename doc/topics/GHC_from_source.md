<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Building GHC from source

:octicons-beaker-24: Experimental

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Stack supports building the GHC compiler from source, using
[Hadrian](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/README.md) (the
build system for GHC). The GHC version to be built and used is defined by a
Git commit ID and a Hadrian "flavour", with the following syntax in a YAML
configuration file:

~~~yaml
compiler: ghc-git-<commit_id>-<Hadrian_flavour>
~~~

In the following example the commit ID is "5be7ad..." and the flavour is
"quick":

~~~yaml
compiler: ghc-git-5be7ad7861c8d39f60b7101fd8d8e816ff50353a-quick
~~~

By default, the code is retrieved from the main GHC repository. If you want to
select another repository, use the `compiler-repository` option in a YAML
configuration file:

~~~yaml
compiler-repository: git://my/ghc/repository
# default
# compiler-repository: https://gitlab.haskell.org/ghc/ghc.git
~~~

Stack does not check the compiler version when it uses a compiler built from
source. It is assumed that the built compiler is recent enough as Stack does not
enable any known workaround to make older compilers work.

Building the compiler can take a very long time (more than one hour). For faster
build times, use Hadrian flavours that disable documentation generation.

!!! note

    The building of the compiler can require the creation of symbolic links
    (symlinks). On Windows, symlinks can only be created by processes with
    Administrator privileges unless Windows' Developer Mode has been set.

### Bootstrap compiler

Building GHC from source requires a working GHC (known as the bootstrap
compiler). As we use a Stack based version of Hadrian (`hadrian/build-stack` in
GHC sources), the bootstrap compiler is configured into `hadrian/stack.yaml` and
fully managed by Stack.

!!! note

    For some commit IDs, the snapshot specified in `hadrian/stack.yaml`
    specifies a version of GHC that cannot be used to build GHC. This results in
    GHC's `configure` script reporting messages similar to the following before
    aborting:

    ~~~text
    checking version of ghc... 9.0.2
    configure: error: GHC version 9.2 or later is required to compile GHC.
    ~~~

    The resolution is: (1) to specify an alternative snapshot (one that
    specifies a sufficiently recent version of GHC) on the command line, using
    Stack's option `--snapshot <snapshot>`. Stack will use that snapshot when
    running GHC's `configure` script; and (2) to set the contents of the `STACK`
    environment variable to be `stack --snapshot <snapshot>`. Hadrian's
    `build-stack` script wil refer to that environment variable for the Stack
    command it uses.

### Hadrian prerequisites

The Hadrian build system has certain
[prerequisites](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/preparation).
It requires certain versions of the `happy` and `alex` executables on the PATH.
Stack will build and install `happy` and `alex`, if not already on the PATH.

=== "macOS"

    Hadrian requires, or case use, certain tools or Python packages that do not
    come with macOS by default and that need to be installed using `brew` or
    `pip3` (Python). Hadrian's LaTeX documentation also requires the
    [DejaVu fonts](https://dejavu-fonts.github.io/) to be installed.

    ~~~zsh
    brew install python@3.11
    # GHC uses a Python script named `boot`.
    brew install automake
    # Tool for generating GNU Standards-compliant Makefiles.
    brew install texinfo
    # Official documentation format of the GNU project.
    pip3 install -U sphinx
    # Sphinx is the Python documentation generator.
    brew install --cask mactex
    # MacTeX: Full TeX Live distribution with GUI applications
    ~~~

=== "Windows"

    Hadrian requires, or can use, certain MSYS2 or Python packages that do not
    come with the Stack-supplied MSYS2 by default and need to be installed
    using `pacman` (MSYS2) or `pip` (Python). Hadrian's LaTeX documentation also
    requires the [DejaVu fonts](https://dejavu-fonts.github.io/) to be
    installed.

    ~~~pwsh
    stack exec -- pacman --sync --refresh
    # Synchronize MSYS2 package databases
    stack exec -- pacman --sync mingw-w64-x86_64-python-pip
    # The PyPA recommended tool (pip) for installing Python packages. Also
    # installs Python as a dependency. GHC uses a Python script named `boot`.
    # The package must be the one from the `mingw64` MSYS2 repository, as Python
    # from the `msys` repository cannot interpret Windows file paths correctly.
    stack exec -- pacman --sync mingw-w64-x86_64-autotools
    # The GNU autotools build system, including `autoreconf`, `aclocal`
    # and `make`. GHC uses a sh script named `configure` which is itself created
    # from a file named `configure.ac`.
    stack exec -- pacman --sync patch
    # A utility to apply patch files to original sources.
    stack exec -- pacman --sync texinfo
    # Utilities to work with and produce manuals, ASCII text, and on-line
    # documentation from a single source file, including `makeinfo`.
    stack exec -- pacman --sync mingw-w64-x86_64-ca-certificates
    # Common CA (certificate authority) certificates.
    stack exec -- pip install -U sphinx
    # Sphinx is the Python documentation generator.
    ~~~

    Hadrian may require certain LaTeX packages and may prompt for these to be
    installed duing the build process.

### Global packages

The GHC compiler you build from sources may depend on unreleased versions of
some global packages (e.g. Cabal). It may be an issue if a package you try to
build with this compiler depends on such global packages because Stack may not
be able to find versions of those packages (on Hackage, etc.) that are
compatible with the compiler.

The easiest way to deal with this issue is to use the
[`drop-packages`](../configure/yaml/project.md#drop-packages)
project-specific configuration option to drop the offending packages as follows.
Instead of using the packages specified in the snapshot, the global packages
bundled with GHC will be used.

~~~yaml
drop-packages:
- Cabal
- ...
~~~

Another way to deal with this issue is to add the relevant packages as
[`extra-deps`](../configure/yaml/project.md#extra-deps) built from source. To
avoid mismatching versions, you can use exactly the same commit id you used to
build GHC as follows:

~~~
extra-deps:
- git: https://gitlab.haskell.org/ghc/ghc.git
  commit: '5be7ad7861c8d39f60b7101fd8d8e816ff50353a'
  subdirs:
    - libraries/Cabal/Cabal
    - libraries/...
~~~
