<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 2. Package description

Let us begin to look at the `helloworld` example in more detail to understand
better how Stack works.

The contents of the project directory are set out below. Click
:material-plus-circle: to learn more about each file or directory:

~~~shell
.
├── .stack-work # (1)!
│   └── ...
│
├── app
│   └── Main.hs # (2)!
├── src
│   └── Lib.hs # (3)!
├── test
│   └── Spec.hs # (4)!
│
├── .gitignore # (5)!
├── CHANGELOG.md # (6)!
├── LICENSE # (7)!
├── README.md # (8)!
│
├── package.yaml # (9)!
├── helloworld.cabal # (10)!
├── Setup.hs # (11)!
│
└── stack.yaml # (12)!
~~~

1.  The Stack work directory for the project and the project package.

    Stack work directories are ones in which Stack stores files created during
    the build process. A product of the build - does not affect the build (other
    than to avoid rebuilding things unnecessarily).

2.  The Haskell source code for the executable (application).

    As your project develops you can add further source code files to the `app`
    directory.

3.  The executable uses a library. The Haskell source code for the library.

    As your project develops you can add further source code files to the `src`
    directory.

4.  The package has a test suite executable. The Haskell source code for the
    test suite.

    As your project develops you can add further source code files to the `test`
    directory.

5.  A text file used to configure the Git tool to ignore certain files. Does not
    affect the build.

6.  A text file in the Markdown format in which changes to the project can be
    documented. Does not affect the build.

7.  A text file used to document the copyright applicable to the project's files
    and the licence for the use of those files. Does not affect the build.

8.  A text file in the Markdown format which is intended to be read by users of
    the project. Does not affect the build.

9.  A file describing the package in the Hpack format. See further below.

10. A file describing the package in the Cabal format. See further below.

11. A Haskell source file which is a component of the Cabal build system. See
    further below.

12. A text file in the YAML format, containing Stack's project-level
    configuration. See the next part of this guide to getting started.

The files of most interest here are `package.yaml` and `helloworld.cabal`. We
will also explain the `Setup.hs` file.

## Package description formats

Each package contains a file that describes the package. It is located in the
package's root directory.

??? question "What is covered by a package description?"

    A package description includes information such as the package name and
    version, and the package's *components*. A package can have an optional
    main library component and optional named sub-library components. It can
    also have optional executable components, test suite components and
    benchmark components. The description identifies other packages on which
    those components depend.

Stack is aware of two different formats of package description, and both files
may be present in the package's root directory:

<div class="grid cards" markdown>

-   :material-package-variant:{ .lg .middle } __Cabal: A Cabal file__

    Used directly by the Cabal build system.

    Unique but simple syntax.

    Named after the package (eg `helloworld.cabal`).

    If no `package.yaml` file, used directly by Stack.

    ---

    Specified by the Cabal project:

    [:octicons-arrow-right-24: Learn more](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html)

-   :material-package-variant-plus:{ .lg .middle } __Hpack: a package.yaml file__

    Used by Stack to create a Cabal file.

    YAML syntax.

    Named `package.yaml`.

    Stack's preferred format. If present, used by Stack.

    ---

    Specified by the Hpack project:

    [:octicons-arrow-right-24: Learn more](https://github.com/sol/hpack?tab=readme-ov-file#documentation)

</div>

??? question "Why use the Hpack format?"

    A `package.yaml` file can be more concise and less repetitive than the Cabal
    file that is generated from it. That is because the Hpack format uses
    defaults and top-level keys common to other parts of the format. The YAML
    syntax, which may already be familiar for some users, can also avoid
    repetition.

    In particular, the format's defaults can infer the names of exposed and
    other modules.

    The format allows a user to specify defaults in a file on GitHub or a local
    file.

??? question "I use the Hpack format. Should I also check-in a Cabal file to my repository?"

    Yes. This helps people who do not use Stack or the Hpack tool separately.

## `package.yaml`

The `package.yaml` file describes the package in the
[Hpack format](https://github.com/sol/hpack?tab=readme-ov-file#documentation).

If a `package.yaml` file is present, Stack will use its built-in Hpack
functionality to create a Cabal file.

??? question "What are the contents of the `package.yaml` file?"

    The contents of the `package.yaml` file for the `helloworld` example are
    described below, using additional YAML comments:

    ~~~yaml
    # The name of the package:
    name:                helloworld
    # The version of the package:
    version:             0.1.0.0
    # The GitHub repository for the package (optional):
    github:              "githubuser/helloworld"
    # The licence for the use of the package's files (optional):
    license:             BSD-3-Clause
    # The author of the package (optional):
    author:              "Author name here"
    # The email address to contact the maintainer of the package (optional):
    maintainer:          "example@example.com"
    # The copyright for the package's files (optional):
    copyright:           "2025 Author name here"

    # Extra files (if any) to be distributed with the source files of the
    # package:
    extra-source-files:
    - README.md
    - CHANGELOG.md

    # Metadata used when publishing your package
    # synopsis:            Short description of your package
    # category:            Web

    # To avoid duplicated efforts in documentation and dealing with the
    # complications of embedding Haddock markup inside cabal files, it is
    # common to point users to the README.md file.
    description:         Please see the README on GitHub at
                         <https://github.com/githubuser/helloworld#readme>

    # Dependencies applicable to all components:
    dependencies:
    - base >= 4.7 && < 5

    # GHC options (if any) common to all components:
    ghc-options:
    # These GHC flags affect which warnings GHC will emit:
    - -Wall
    - -Wcompat
    - -Widentities
    - -Wincomplete-record-updates
    - -Wincomplete-uni-patterns
    - -Wmissing-export-lists
    - -Wmissing-home-modules
    - -Wpartial-fields
    - -Wredundant-constraints

    # The main (unnamed) library component of the package (if it has one):
    library:
      # Directories containing source files:
      source-dirs: src

    # The executable components of the package (if it has any):
    executables:
      # The executable component named 'helloworld-exe':
      helloworld-exe:
        # The source file exporting the 'main' function:
        main:                Main.hs
        # Directories containing source files:
        source-dirs:         app
        # GHC options applicable to the component:
        ghc-options:
        # Link the program with the 'threaded' version of GHC's runtime system:
        - -threaded
        # Make all of GHC's runtime system (RTS) options available:
        - -rtsopts
        # Compile so as to use simultaneous threads when running the program,
        # based on how many processors are in the machine.
        - -with-rtsopts=-N
        # Dependencies applicable to the component:
        dependencies:
        # The main library of the package:
        - helloworld

    # The test suite components of the package (if it has any). Test suites have
    # keys in common with executables:
    tests:
      # The test suite component named 'helloworld-test':
      helloworld-test:
        main:                Spec.hs
        source-dirs:         test
        ghc-options:
        - -threaded
        - -rtsopts
        - -with-rtsopts=-N
        dependencies:
        - helloworld
    ~~~

??? question "What are the contents of a minimal `package.yaml` file?"

    For a package `my-package-0.1.0.0` with a main library, an executable
    named `my-program`, and a dependency only on the `base` package, its
    `package.yaml` file could be as simple as the one below:

    ~~~yaml
    package: my-package
    version: 0.1.0.0
    dependencies:
    - base
    library:
      source-dirs: src
    executables:
      my-program:
        main: Main.hs
        source-dirs: app
        ghc-options:
        - -threaded
        - -rtsopts
        - -with-rtsopts=-N
        dependencies:
        - my-package
    ~~~

## `helloworld.cabal`

In the case of the `helloworld` example, the `helloworld.cabal` file is updated
automatically as part of the `stack build` process and should not be modified.

If the `package.yaml` file were deleted, Stack would use the Cabal file
directly.

??? question "What are the contents of the `helloworld.cabal` file?"

    The contents of the `helloworld.cabal` file are described below, using
    additional Cabal file comments:

    ~~~text
    -- The version of the Cabal package description format specification:
    cabal-version: 2.2

    -- This file has been generated from package.yaml by hpack version 0.37.0.
    --
    -- see: https://github.com/sol/hpack

    -- The name of the package:
    name:           helloworld
    -- The version of the package:
    version:        0.1.0.0
    -- The description of the package:
    description:    Please see the README on GitHub at
                    <https://github.com/githubuser/helloworld#readme>
    -- A URL for the package:
    homepage:       https://github.com/githubuser/helloworld#readme
    -- A URL for bug reports for the package:
    bug-reports:    https://github.com/githubuser/helloworld/issues
    -- The author of the package:
    author:         Author name here
    -- The email address to contact the maintainer of the package:
    maintainer:     example@example.com
    -- The copyright for the package's files:
    copyright:      2025 Author name here
    -- The licence for the use of the package's files:
    license:        BSD-3-Clause
    -- The file documenting the terms of the licence:
    license-file:   LICENSE
    -- The Cabal system build type of the package:
    build-type:     Simple
    -- Extra files to be distributed with the source files of the package:
    extra-source-files:
        README.md
        CHANGELOG.md

    -- The respository for the package:
    source-repository head
      type: git
      location: https://github.com/githubuser/helloworld

    -- The main (unnamed) library component of the package:
    library
      -- The modules that the library exposes:
      exposed-modules:
          Lib
      -- The other modules of the compoment:
      other-modules:
          Paths_helloworld
      -- Automatically generated modules of the component:
      autogen-modules:
          Paths_helloworld
      -- Directories containing source files:
      hs-source-dirs:
          src
      -- GHC options applicable to the component. In this case, they are flags
      -- that affect which warnings GHC will emit:
      ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates
                   -Wincomplete-uni-patterns -Wmissing-export-lists
                   -Wmissing-home-modules -Wpartial-fields
                   -Wredundant-constraints
      -- Dependencies applicable to the building of the component:
      build-depends:
          base >=4.7 && <5
      -- The applicable version of the Haskell language:
      default-language: Haskell2010

    -- The executable 'helloworld-exe' component of the package. Executable
    -- components have fields in common with library components:
    executable helloworld-exe
      -- The source file exporting the 'main' function:
      main-is: Main.hs
      other-modules:
          Paths_helloworld
      autogen-modules:
          Paths_helloworld
      hs-source-dirs:
          app
      -- GHC options applicable to the component. In this case, they include
      -- flags that affect GHC's runtime system (RTS).
      ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates
                   -Wincomplete-uni-patterns -Wmissing-export-lists
                   -Wmissing-home-modules -Wpartial-fields
                   -Wredundant-constraints -threaded -rtsopts -with-rtsopts=-N
      build-depends:
          base >=4.7 && <5
        , helloworld
      default-language: Haskell2010

    -- The test suite 'helloworld-test' component of the package. Test suite
    -- components have fields in common with executable components:
    test-suite helloworld-test
      -- The type of the test suite:
      type: exitcode-stdio-1.0
      main-is: Spec.hs
      other-modules:
          Paths_helloworld
      autogen-modules:
          Paths_helloworld
      hs-source-dirs:
          test
      ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates
                   -Wincomplete-uni-patterns -Wmissing-export-lists
                   -Wmissing-home-modules -Wpartial-fields
                   -Wredundant-constraints -threaded -rtsopts -with-rtsopts=-N
      build-depends:
          base >=4.7 && <5
        , helloworld
      default-language: Haskell2010
    ~~~

## `Setup.hs`

The `Setup.hs` file is a component of the Cabal build system.

Technically, it is not needed by Stack, but it is considered good practice to
include it. The file we're using is boilerplate:

~~~haskell
import Distribution.Simple
main = defaultMain
~~~
