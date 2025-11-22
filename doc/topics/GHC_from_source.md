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

The [`-j`, `--jobs` option](../configure/global_flags.md#-jobs-or-j-option) at
the command line or the [`jobs`](../configure/yaml/non-project.md#jobs) option
in a YAML configuraton file can be used to specify Hadrian's `-j[<n>]` flag.

By default, the code is retrieved from the main GHC repository. If you want to
select another repository, use the `compiler-repository` option in a YAML
configuration file:

~~~yaml
compiler-repository: git://my/ghc/repository
# default
# compiler-repository: https://gitlab.haskell.org/ghc/ghc.git
~~~

By default, the Hadrian build target is `reloc-binary-dist` on Windows and
`binary-dist` on other operating systems. If you want to specify another
Hadrian build target, use the `compiler-target` option in a YAML configuration
file:

~~~yaml
compiler-target: binary-dist
# default (Windows)
# compiler-target: reloc-binary-dist
# default (non-Windows)
# compiler-target: binary-dist
~~~

By default, Stack assumes that the path to the binary distribution built by
Hadrian is `_build/reloc-bindist` on Windows and `_build/bindist` on other
operating systems. If you want to specify another path, use the
`compiler-bindist-path` option in a YAML configuration file:

~~~yaml
compiler-bindist-path: _build/bindist
# default (Windows)
# compiler-bindist-path: _build/reloc-bindist
# default (non-Windows)
# compiler-bindist-path: _build/bindist
~~~

!!! note

    The Hadrian build target `reloc-binary-dist` was introduced with Git commit
    id
    [`fe23629b147d419053052e6e881f6e8ddfbf3bae`](https://gitlab.haskell.org/ghc/ghc/-/commit/fe23629b147d419053052e6e881f6e8ddfbf3bae).

    Once introduced, the target must be used on Windows.

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

    The resolution is:

    1.  to specify an alternative snapshot (one that specifies a sufficiently
        recent version of GHC) on the command line, using Stack's option
        `--snapshot <snapshot>`. Stack will use that snapshot when running GHC's
        `configure` script; and

    2.  to set the contents of the `STACK` environment variable to be
        `stack --snapshot <snapshot>`. If `<snapshot>` is a path to a local YAML
        file, it needs to be an absolute one. Hadrian's `build-stack` script
        will refer to that environment variable for the Stack command it uses.

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
    stack exec -- pacman -sync mingw-w64-x86_64-python-sphinx
    # Sphinx is the Python documentation generator.
    stack exec -- pacman -sync mingw-w64-x86_64-texlive-full
    # The TeX Live distribution.
    ~~~

    Hadrian may require certain LaTeX packages and may prompt for these to be
    installed duing the build process.

    !!! note

        Before commit
        [cdddeb0f1280b40cc194028bbaef36e127175c4c](https://gitlab.haskell.org/ghc/ghc/-/commit/cdddeb0f1280b40cc194028bbaef36e127175c4c)
        the GHC project did not support `autoconf >= 2.72`.

        MSYS2 can be
        [configured](https://www.msys2.org/docs/autotools/#autoconf-wrapper) to
        use an earlier version of `autoconf` than the latest version.

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
