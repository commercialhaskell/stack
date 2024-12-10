<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Frequently asked questions

## Stack and Cabal

??? question "What is the relationship between Stack and Cabal?"

    'Cabal' can refer to Cabal (the library) or to Cabal (the tool).

    === "Cabal (the library)"

         Cabal (the library) is used by Stack to build your Haskell code.

         A Haskell package is described by a Cabal file, which file is part of
         the package. The file is named `<package_name>.cabal`.

         Stack requires a project-level configuration file (`stack.yaml`, by
         default).

         For further information about the difference between a Cabal file and
         a project-level configuration file, see the
         [stack.yaml vs a Cabal file](topics/stack_yaml_vs_cabal_package_file.md)
         documentation.

         The [`stack init`](commands/init_command.md) command initializes a
         project-level configuration file from package description files.

         Stack uses Cabal (the library) via an executable. For
         `build-type: Simple` (the most common case), Stack builds that
         executable using the version of Cabal which came with GHC. Stack caches
         such executables, in the [Stack root](topics/stack_root.md) under
         directory `setup-exe-cache`.

         In rare or complex cases, a different version of Cabal to the one that
         came with GHC may be needed. `build-type: Custom` and a `setup-custom`
         stanza in the Cabal file, and a `Setup.hs` file in the package
         directory, can be specified. Stack's project-level configuration file
         can then specify the version of Cabal that Stack will use to build the
         executable (named `setup`) from `Setup.hs`. Stack will use Cabal via
         `setup`.

    === "Cabal (the tool)"

        Cabal (the tool) is a tool provided by the
        [`cabal-install`](https://hackage.haskell.org/package/cabal-install)
        Haskell package. It aims to simplify the process of managing Haskell
        software by automating the fetching, configuration, compilation and
        installation of Haskell libraries and programs. These are goals that
        Stack shares. Stack can be used independently of Cabal (the tool) but
        users can also use both, if they wish.

??? question "How do I use Stack with sandboxes?"

    A 'sandbox' is a development environment that is isolated from other parts
    of the system. The concept of sandboxing is built in to Stack. All builds
    are automatically isolated into separate package databases.

??? question "Can I run `cabal` commands inside `stack exec`?"

    Yes. Some `cabal` commands are inconsistent with the `GHC_PACKAGE_PATH`
    environment variable in the Stack environment. Command, for example:

    ~~~text
    stack exec --no-ghc-package-path -- cabal build
    ~~~

## GHC or GHCi-related

??? question "Will Stack interfere with the GHC I already have installed?"

    No.

??? question "I already have GHC installed. Can I still use Stack?"

    Yes. In its default configuration, Stack will simply ignore any system GHC
    installation and use a sandboxed GHC that it has installed itself. You can
    find these sandboxed GHC installations in the `ghc-*` directories in the
    `stack path --programs` directory.

    If you would like Stack to use your system GHC installation, use the
    [`--system-ghc`](configure/yaml/non-project.md#system-ghc) flag or run
    `stack config set system-ghc --global true` to make Stack check your PATH
    for a suitable GHC by default.

    Stack can only use a system GHC installation if its version is compatible
    with the configuration of the current project, particularly the snapshot
    specified by the [`snapshot`](configure/yaml/project.md#snapshot) or
    [`resolver`](configure/yaml/project.md#resolver) key.

    GHC installation doesn't work for all operating systems, so in some cases
    you will need to use `system-ghc` and install GHC yourself.

??? question "When I command `stack ghci` what version of GHC is used?"

    The version of GHC is specified by the snapshot in the relevant Stack
    project-level configuration file. This may be the file in the
    `global-project` directory in the [Stack root](topics/stack_root.md).

    For further information, see the [configuration](configure/yaml/index.md)
    documentation.

??? question "How does Stack determine what GHC to use?"

    In its default configuration, Stack determines from the current project which
    GHC version, architecture etc it needs. It then looks in the `ghc-<version>`
    subdirectory of the `stack path --programs` directory for a compatible GHC,
    requesting to install one via `stack setup` if none is found.

    If you are using the [`--system-ghc`](configure/yaml/non-project.md#system-ghc)
    flag or have configured `system-ghc: true` either in the project `stack.yaml` or
    the global `config.yaml`, Stack will use the first GHC that it finds on your
    PATH, falling back on its sandboxed installations only if the found GHC doesn't
    comply with the various requirements (version, architecture) that your project
    needs.

    See issue [#420](https://github.com/commercialhaskell/stack/issues/420) for a
    detailed discussion of Stack's behavior when `system-ghc` is enabled.

??? question "How can I test that different GHC versions can build my project?"

    You can create multiple project-level configuration files for your project,
    one for each build plan. For example, you might set up your project
    directory like so:

    ~~~text
    myproject/
      stack-ghc-9.8.4.yaml
      stack-ghc-9.8.4.yaml
      stack.yaml --> symlink to stack-ghc-9.8.4.yaml
      myproject.cabal
      src/
        ...
    ~~~

    When you run `stack build`, you can set the `STACK_YAML` environment
    variable to indicate which build plan to use. Command:

    === "Unix-like"

        ~~~text
        STACK_YAML=stack-ghc-9.8.4.yaml
        stack build
        ~~~

    === "Windows"

        ~~~text
        $Env:STACK_YAML='stack-ghc-9.8.4.yaml'
        stack build
        ~~~

    === "Windows (Command Prompt)"

        ~~~text
        set STACK_YAML=stack-ghc-9.8.4.yaml
        stack build
        ~~~

## Setup-related

??? question "Where is Stack installed?"

    Command:

    ~~~text
    stack uninstall
    ~~~

    for information about where Stack is installed.

??? question "Can I change Stack's default temporary directory?"

    Stack downloads and extracts files to `$STACK_ROOT/programs` on most platforms,
    which defaults to `~/.stack/programs`. On Windows `$LOCALAPPDATA\Programs\stack`
    is used. If there is not enough free space in this directory, Stack may fail.
    For instance, `stack setup` with a GHC installation requires roughly 1GB free.
    If this is an issue, you can set `local-programs-path` in your
    `~/.stack/config.yaml` to a directory on a file system with more free space.

    If you use Stack with Nix integration, be aware that Nix uses a `TMPDIR`
    variable, and if it is not set Nix sets it to some subdirectory of `/run`, which
    on most Linuxes is a Ramdir. Nix will run the builds in `TMPDIR`, therefore if
    you don't have enough RAM you will get errors about disk space. If this happens
    to you, please _manually_ set `TMPDIR` before launching Stack to some directory
    on the disk.

??? question "On Windows, `stack setup` tells me to add certain paths to the PATH instead of doing it?"

    In PowerShell, it is easy to automate even that step. Command:

    ~~~text
    $Env:Path = ( stack setup | %{ $_ -replace '[^ ]+ ', ''} ), $Env:Path -join ";"
    ~~~

??? question "Does Stack install the system/C libraries that some Cabal packages depend on?"

    No. This is currently out of the scope of Stack's target set of features.
    Instead of attempting to automate the installation of 3rd party dependencies, we
    have the following approaches for handling system dependencies:

    * Nix and docker help make your build and execution environment deterministic
      and predictable. This way, you can install system dependencies into a
      container, and share this container with all developers.

    * If you have installed some libraries into a non-standard location, use the
      [`extra-lib-dirs`](configure/yaml/non-project.md#extra-lib-dirs) option or the
      [`extra-include-dirs`](configure/yaml/non-project.md#extra-include-dirs)
      option to specify it.

    In the future, Stack might give operating system-specific suggestions for how to
    install system libraries.

??? question "How can I make Stack aware of my custom SSL certificates?"

    === "Linux"

        Use the `SYSTEM_CERTIFICATE_PATH` environment variable to point at the directory
        where you keep your SSL certificates.


    === "macOS"

        In principle, you can use the following command to add a certificate to your
        system certificate keychain:

        ~~~bash
        sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain <certificate>
        ~~~

        Some users have reported issues with this approach, see issue
        [#907](https://github.com/commercialhaskell/stack/issues/907) for more
        information.

## Package description format-related

??? question "How does Stack support the Hpack specification?"

    The [Hpack](https://github.com/sol/hpack) package description format is an
    alternative to that used in a Cabal file.

    If a package directory contains an package description file in the Hpack
    format (`package.yaml`), Stack will use that file to create the
    corresponding Cabal file.

    [`stack init`](commands/init_command.md) will use Hpack format package
    description files, if they are present.

    The [`with-hpack`](configure/yaml/non-project.md#with-hpack) non-project
    specific configuration option or the
    [`--with-hpack`](configure/global_flags.md#-with-hpack-option) global flag
    can be used to specify an Hpack executable to use instead of Stack's
    built-in Hpack functionality.

## Package index-related

??? question "How do I update my package index?"

    Command:

    ~~~text
    stack update
    ~~~

    However, generally, it's not necessary with Stack: if the package index is
    missing, or if a snapshot refers to package version that isn't available,
    Stack will automatically update the package index and then try again.

    If you run into a situation where Stack doesn't automatically update the
    package index, please report it as a bug.

??? question "Is it dangerous to update the package index automatically? Can that corrupt build plans?"

    No. Stack is explicit about which packages it's going to build. There are
    three sources of information to tell Stack which packages to install: the
    selected snapshot, the `extra-deps` configuration value, and your project
    packages. The only way to get Stack to change its build plan is to modify
    one of those three. Updating the index will have no effect on Stack's
    behavior.

??? question "How do I use a custom package index?"

    See the [`package-index`](configure/yaml/non-project.md#package-index)
    non-project specific configuration option documentation.

## Package-related

??? question "How do I use a package version on Hackage not in a snapshot?"

    Add the package version to the [`extra-deps`](configure/yaml/project.md)
    project-specific configuration option in the
    [project-level configuration file](configure/yaml/index.md).

??? question "How do I use a package version not on Hackage?"

    Add the location of the package version to the
    [`extra-deps`](configure/yaml/project.md) project-specific configuration
    option in the [project-level configuration file](configure/yaml/index.md).

    For further information, see the
    [package location](topics/package_location.md) documentation.

??? question "How do I use a modified version of a package?"

    Typically, a modified version of a package is used as a project package.
    Add the location of the package to the
    [`packages`](configure/yaml/project.md#packages) project-specific
    configuration option in the
    [project-level configuration file](configure/yaml/index.md).

    One way to get the source code for the unmodified package version is to use
    the [`stack unpack`](commands/unpack_command.md).

??? question "I'd like to use my installed packages in a different directory. How do I tell Stack where to find my packages?"

    Set the `STACK_YAML` environment variable to point to the `stack.yaml`
    configuration file for your project. Then you can run `stack exec`, `stack ghc`,
    etc., from any directory and still use your packages.

## `stack build`-related

??? question "Why does `stack build` fail with GHC 9.8.1 and 9.8.2 only?"

    If the Cabal file of the relevant package makes use of a `c-sources` field,
    the failure may be due to `Cabal-3.10.2.0` enforcing that the field can
    specify only `*.c` files. Earlier and later versions of Cabal (the library)
    tolerate other files.

    When the Cabal build type is not `Custom`, Stack builds with the `Cabal`
    boot package of the specified version of GHC. The boot package of GHC 9.8.1
    and 9.8.2 is `Cabal-3.10.2.0`.

??? question "What causes dependency on multiple versions of the same package?"

    When building a package, during its configuration, Stack may warn:

    ~~~text
    This package indirectly depends on multiple versions of the same package.
    This is very likely to cause a compile failure.
    ~~~

    and the build subsequently fails.

    Often the cause is that: (1) the package depends, directly or indirectly, on
    a GHC wired-in package (for example, the `ghc` package); and (2) a direct or
    indirect dependency of that wired-in package is also specified as an
    extra-dep but the versions differ.

??? question "Why does `stack test` trigger a rebuild of other components?"

    If the set of dependencies of a project package to be built are not a
    subset of the set of dependencies when it was last built, then that will
    trigger a rebuild of components that were previously built.

    The command:

    ~~~text
    stack build
    ~~~

    will build the library and executable components of project packages and the
    build will take into account the dependencies of those components.

    If you then command:

    ~~~text
    stack test
    ~~~

    or, equivalently:

    ~~~text
    stack build --test
    ~~~

    the test suite components of project packages are added to the build
    targets.

    That can add dependencies to a project package, if its test suite
    components have dependencies that are not dependencies of its library
    and executable components.

    What is true of test suite components applies equally to benchmark
    components.

    If that behaviour is undesirable, a way to avoid it is to change the
    description of each project package so that adding its test suite (or
    benchmark) components does not add dependencies to the package. That is,
    to specify, in the package description, the dependencies as common to all
    the components that you are switching between from one build to another.

    For example, if you are using `package.yaml`, add the dependencies to its
    top-level `dependencies` key.

    Alternatively, build all components of project packages without running
    tests or benchmarks once built. Add the following to a configuration file:

    ~~~yaml
    build:
      test: true
      test-arguments:
        no-run-tests: true
      bench: true
      benchmark-opts:
        no-run-benchmarks: true
    ~~~

    or command:

    ~~~text
    stack build --test --no-run-tests --bench --no-run-benchmarks
    ~~~

??? question "How do I use a custom preprocessor?"

    See the
    [`customer-prepocessor-extensions`](configure/yaml/project.md#custom-preprocessor-extensions)
    project-specific configuration option documentation.

??? question "How do I get extra tools used during building?"

    Stack will automatically install tools used during building required by your
    packages or their dependencies, in particular
    [Alex](https://hackage.haskell.org/package/alex) and
    [Happy](https://hackage.haskell.org/package/happy).

    !!! note

        This works when using LTS or nightly snapshots, not with GHC or custom
        snapshots. You can manually install tools used during building by running,
        e.g., `stack build alex happy`.

??? question "My tests are failing. What should I do?"

    Like all other targets, `stack test` runs test suites in parallel by default.
    This can cause problems with test suites that depend on global resources such
    as a database or binding to a fixed port number. A quick hack is to force stack
    to run all test suites in sequence, using `stack test --jobs=1`. For test
    suites to run in parallel developers should ensure that their test suites do
    not depend on global resources (e.g. by asking the operating system for a random
    port to bind to) and where unavoidable, add a lock in order to serialize access
    to shared resources.

??? question "How do I use Stack with Docker?"

    See the [Docker integration](topics/docker_integration.md) documentation.

??? question "How do I build a statically-linked executable on Linux?"

    The way that Stack itself builds statically-linked Stack executables for Linux
    is as follows:

    * In the Cabal file, the following
      [`ld-options`](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#pkg-field-ld-options)
      are set: `-static` and `-pthread`.

    * The Stack command is run in a Docker container based on Alpine Linux. The
      relevant Docker image repository is set out in Stack's `stack.yaml` file. See
      also Olivier Benz's [GHC musl project](https://gitlab.com/benz0li/ghc-musl).

    * Stack's configuration includes:

        ~~~yaml
        extra-include-dirs:
        - /usr/include
        extra-lib-dirs:
        - /lib
        - /usr/lib
        ~~~

    * The build command is `stack build --docker --system-ghc --no-install-ghc` (on
      x86_64) or
      `stack build --docker --docker-stack-exe=image --system-ghc --no-install-ghc`
      (on AArch64; the host Stack and the image Stack must have the same version
      number).

??? question "Why doesn't Stack rebuild my project when I specify `--ghc-options` on the command line?"

    Because GHC options often only affect optimization levels and warning behavior,
    Stack doesn't recompile when it detects an option change by default. This
    behavior can be changed though by setting the
    [`rebuild-ghc-options` option](configure/yaml/non-project.md#rebuild-ghc-options)
    to `true`.

    To force recompilation manually, use the `--force-dirty` flag. If this still
    doesn't lead to a rebuild, add the `-fforce-recomp` flag to your
    `--ghc-options`.

??? question "Why doesn't Stack apply my `--ghc-options` to my dependencies?"

    By default, Stack applies command line GHC options only to
    [project packages](configure/yaml/project.md#packages). For an explanation of
    this choice see this discussion on issue
    [#827](https://github.com/commercialhaskell/stack/issues/827#issuecomment-133263678).

    If you still want to set specific GHC options for a dependency, use the
    [`ghc-options`](configure/yaml/non-project.md#ghc-options) option in your YAML
    configuration file.

    To change the set of packages that command line GHC options apply to, use the [`apply-ghc-options`](configure/yaml/non-project.md#apply-ghc-options) option.

??? question "How does Stack handle parallel builds?"

    See issue [#644](https://github.com/commercialhaskell/stack/issues/644) for more
    details.

??? question "Where does the output from `--ghc-options=-ddump-splices` (and other `-ddump*` options) go?"

    These are written to `*.dump-*` files inside the package's `.stack-work`
    directory. Specifically, they will be available at
    `PKG-DIR/$(stack path --dist-dir)/build/SOURCE-PATH`, where `SOURCE-PATH` is the
    path to the source file, relative to the location of the Cabal file. When
    building named components such as test-suites, `SOURCE-PATH` will also include
    `COMPONENT/COMPONENT-tmp`, where `COMPONENT` is the name of the component.

??? question "Why is DYLD_LIBRARY_PATH ignored?"

    If you are on Mac OS X 10.11 ("El Capitan") or later, there is a GHC issue
    [#11617](https://ghc.haskell.org/trac/ghc/ticket/11617) which prevents the
    `DYLD_LIBRARY_PATH` environment variable from being passed to GHC (see issue
    [#1161](https://github.com/commercialhaskell/stack/issues/1161)) when System
    Integrity Protection (a.k.a. "rootless") is enabled. There are two known
    workarounds:

     1. Known to work in all cases:
        [disable System Integrity Protection](http://osxdaily.com/2015/10/05/disable-rootless-system-integrity-protection-mac-os-x/).
        **WARNING: Disabling SIP will severely reduce the security of your system, so only do this if absolutely necessary!**
     2. Experimental: modify GHC's shell script wrappers to use a shell outside the
        protected directories (see issue
        [#1161](https://github.com/commercialhaskell/stack/issues/1161#issuecomment-186690904)).

??? question "How do I get `verbose` output from GHC when I build?"

    Set the [`--ghc-options`](commands/build_command.md#-ghc-options-option)
    option of `stack build` to `-v`.

## Snapshot-related

??? question "How does Stack choose which snapshot to use when creating a project-level configuration file?"

    See the [`stack init`](commands/init_command.md) command documentation.

## CI-related

??? question "How do I use Stack with Travis CI?"

    See the [Travis CI](topics/travis_ci.md) documentation.

??? question "How do I use Stack with Azure CI?"

    See the [Azure CI](topics/azure_ci.md) documentation.

## Linux-related

??? question "How do fix error [S-9443] for 'linux64-ncurses6'?"

    Most Linux distributions have standardized on providing `libtinfo.so.6`,
    either directly or as a symbolic link to `libncursesw.so.6`. As such, there
    are no GHC binary distributions that link to `libncursesw.so.6` after
    GHC 8.2.2.

    This error can be prevented by creating a symbolic link to
    `libncursesw.so.6` using name `libtinfo.so.6`. Command:

    ~~~bash
    ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6
    ~~~

    Root privileges may be required.

## macOS-related

??? question "On macOS, how do I resolve linker errors when running `stack setup` or `stack build`?"

    This is likely to be caused by having both a LLVM installation and default
    Apple Clang compiler on the PATH. The symptom of this issue is a linker
    error "bad relocation (Invalid pointer diff)". The compiler picks up
    inconsistent versions of binaries and the mysterious error occurs.

    The workaround is to remove LLVM binaries from the PATH.

??? question "On macOS, how do I suppress `'-nopie'` warnings with `stack build`?"

    ~~~bash
    clang: warning: argument unused during compilation: '-nopie'
     [-Wunused-command-line-argument]
    ~~~

    This warning is shown when compiler support of `-no-pie` is expected but
    unavailable. It's possible to bypass the warning for a specific version of GHC
    by modifying a global setting:

    ~~~bash
    # ~/.stack/programs/x86_64-osx/ghc-8.2.2/lib/ghc-8.2.2/settings
    -- ("C compiler supports -no-pie", "YES"),
    ++ ("C compiler supports -no-pie", "NO"),
    ~~~

    **Note that we're fixing `ghc-8.2.2` in this case; repeat for other versions as necessary.**
    You should apply this fix for the version of GHC that matches your snapshot.

    Issue [#4009](https://github.com/commercialhaskell/stack/issues/4009) goes into
    further detail.

## Windows-related

??? question "What is licensing restrictions on Windows?"

    Currently, on Windows, GHC produces binaries linked statically with
    [GNU Multiple Precision Arithmetic Library](https://gmplib.org/) (GMP), which is
    used by [integer-gmp](https://hackage.haskell.org/package/integer-gmp) library
    to provide big integer implementation for Haskell. Contrary to the majority of
    Haskell code licensed under permissive BSD3 license, GMP library is licensed
    under LGPL, which means resulting binaries
    [have to be provided with source code or object files](http://www.gnu.org/licenses/gpl-faq.html#LGPLStaticVsDynamic).
    That may or may not be acceptable for your situation. Current workaround is to
    use GHC built with alternative big integer implementation called
    `integer-simple`, which is free from LGPL limitations as it's pure Haskell and
    does not use GMP.  Unfortunately it has yet to be available out of the box with
    Stack. See issue [#399](https://github.com/commercialhaskell/stack/issues/399)
    for the ongoing effort and information on workarounds.

??? question "I have a Windows username with a space in it and problems building"

    See the [`local-programs-path`](configure/yaml/non-project.md#local-programs-path)
    non-project specific configuration option documentation for advice.

??? question "How to get a working executable on Windows?"

    When executing a binary after building with `stack build` (e.g. for target
    "foo"), the command `foo.exe` might complain about missing runtime libraries
    (whereas `stack exec foo` works).

    Windows is not able to find the necessary C++ libraries from the standard
    prompt because they're not in the PATH environment variable. `stack exec` works
    because it's modifying PATH to include extra things.

    Those libraries are shipped with GHC (and, theoretically in some cases, MSYS2).
    The easiest way to find them is `stack exec which`. For example, command:

    ~~~text
    stack exec -- which libstdc++-6.dll
    /c/Users/Michael/AppData/Local/Programs/stack/i386-windows/ghc-7.8.4/mingw/bin/libstdc++-6.dll
    ~~~

    A quick workaround is adding this path to the PATH environment variable or
    copying the files somewhere Windows finds them (see
    https://msdn.microsoft.com/de-de/library/7d83bc18.aspx).

    See issue [#425](https://github.com/commercialhaskell/stack/issues/425).
