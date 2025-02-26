<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Project-specific configuration

Project-specific configuration options are valid only in a project-level
configuration file (`stack.yaml`, by default). Most of Stack's configuration
options are [non-project specific](non-project.md).

Each of the Haskell packages to which a Stack project relates is either a
**project package** that is part of the project and located locally or a package
on which one or more of the project packages depends (directly or indirectly).
The latter is referred to as a **dependency** and it may be located locally or
elsewhere.

!!! info

    Project packages are built by default. Dependencies are only built when
    needed. Building can target individual components of a project package. The
    individual components of dependencies cannot be targeted. Test suite and
    benchmark components of a project package can be built and run. The library
    and executable components of a dependency, and only those components, are
    built when the dependency is needed.

In your project-specific options, you specify both **which project packages** to
build and **which dependencies to use** when building these packages.

A dependency specified as an [extra-dep](#extra-deps) will shadow a package of
the same name specified in a [snapshot](#snapshot). A project package will
shadow a dependency of the same name.

## snapshot

Command line equivalent (takes precedence):
[`--snapshot`](../global_flags.md#-snapshot-option) or
[`--resolver`](../global_flags.md#-resolver-option) option

The `snapshot` key specifies which snapshot is to be used for this project. A
snapshot defines a GHC version, the package version of packages available for
installation, and various settings like build flags. It is also called a
resolver since a snapshot states how dependencies are resolved. There are
currently four snapshot types:

* LTS Haskell snapshots, e.g. `snapshot: lts-23.0`
* Stackage Nightly snapshots, e.g. `snapshot: nightly-2024-12-13`
* No snapshot, just use packages shipped with the compiler. For GHC this looks
  like `snapshot: ghc-9.8.4`
* Custom snapshot, via a URL or relative file path. For further information, see
  the [snapshot location](../../topics/snapshot_location.md) documentation.

Each of these snapshots will also determine what constraints are placed on the
compiler version. See the [compiler-check](non-project.md#compiler-check) option
for some additional control over compiler version.

A package version specified in a snapshot can be shadowed by an
[extra-dep](#extra-deps) of the same name or a [project package](#packages) of
the same name.

## resolver

`resolver` and [`snapshot`](#snapshot) are synonyms. Only one of these keys is
permitted, not both.

## packages

Default:

~~~yaml
packages:
- .
~~~

The `packages` key specifies a list of the project packages that are part of
your project. These are specified via paths to local directories. A path is
considered relative to the directory containing the project-level configuration
file (`stack.yaml`, by default). For example, if the `stack.yaml` file is
located at `/dir1/dir2/stack.yaml`, and has:

~~~yaml
packages:
- my-package
- dir3/my-other-package
~~~

the configuration means "project packages in directories `/dir1/dir2/my-package`
and `/dir1/dir2/dir3/my-other-package`".

The `packages` key is optional. The default value, '`.`', means that the
project has a single project package located in the current directory.

A project package will shaddow a dependency of the same name.

A package version specified in a snapshot can be shadowed by an
[extra-dep](#extra-deps) of the same name or a [project package](#packages) of
the same name.

Each specified project package directory must have a valid Cabal file or Hpack
`package.yaml` file present. Any subdirectories of the directory are not
searched for Cabal files. A subdirectory has to be specified as an independent
item in the list of project packages.

A project package is different from a dependency (located locally or elsewhere)
specified as an [extra-dep](#extra-deps) or via a [snapshot](#snapshot). For
example:

* a project package will be built by default by commanding
  [`stack build`](../../commands/build_command.md) without specific targets. A
  dependency will only be built if it is needed; and
* test suites and benchmarks may be built and run for a project package. They
  are never run for a dependency.

## extra-deps

Default: `[]`

The `extra-deps` key specifies a list of extra dependencies on top of what is
defined in the [snapshot](#snapshot). A dependency may come from either a Pantry
package location or a local file path.

A Pantry package location is one or three different kinds of sources:

* the package index (Hackage);
* an archive (a tarball or zip file, either local or over HTTP or HTTPS); or
* a Git or Mercurial repository.

For further information on the format for specifying a Pantry package location,
see the [package location](../../topics/package_location.md) documentation. For
example:

~~~yaml
extra-deps:
# The latest revision of a package in the package index (Hackage):
- acme-missiles-0.3
# A specific revision of a package in the package index (Hackage):
- acme-missiles-0.3@rev:0
# An *.tar.gz archive file over HTTPS:
- url: https://github.com/example-user/my-repo/archive/08c9b4cdf977d5bcd1baba046a007940c1940758.tar.gz
  subdirs:
  - my-package
# A Git repository at a specific commit:
- git: https://github.com/example-user/my-repo.git
  commit: '08c9b4cdf977d5bcd1baba046a007940c1940758'
# An archive of files at a point in the history of a GitHub repository
# (identified by a specific commit):
- github: example-user/my-repo
  commit: '08c9b4cdf977d5bcd1baba046a007940c1940758'
  subdirs:
  - my-package
~~~

!!! note

    GHC boot packages are special. An extra-dep with the same package name and
    version as a GHC boot package will be ignored.

!!! note

    The `commit:` key expects a YAML string. A commit hash, or partial hash,
    comprised only of digits represents a YAML number, unless it is enclosed in
    quotation marks.

For a local file path source, the path is considered relative to the directory
containing the `stack.yaml` file. For example, if the `stack.yaml` is located
at `/dir1/dir2/stack.yaml`, and has:

~~~yaml
extra-deps:
- my-package
- dir3/my-other-package
~~~

the configuration means "extra-deps packages in directories
`/dir1/dir2/my-package` and `/dir1/dir2/dir3/my-other-package`".

!!! note

    A local file path that has the format of a package identifier will be
    interpreted as a reference to a package on Hackage. Prefix it with `./` to
    avoid that confusion.

!!! note

    A specified extra-dep that does not have the format of a valid Pantry
    package location (for example, a reference to a package on Hackage that
    omits the package's version) will be interpreted as a local file path.

An extra-dep will shadow a dependency specified in a [snapshot](#snapshot) of
the same name. An extra-dep can be shadowed by a [project package](#packages) of
the same name.

!!! info "GHC wired-in packages"

    Some Haskell packages published on Hackage, for example `base` and `ghc`,
    are referred to as 'wired-in' to one or more versions of GHC or as 'magic'.
    They can be distinguished from normal packages by the contents of their
    Cabal files: GHC's `-this-unit-id` option is set as the name of the package
    without a version. For example, the `base.cabal` for `base-4.19.1.0`
    includes:

    ~~~yaml
    -- We need to set the unit id to base (without a version number)
    -- as it's magic.
    ghc-options: -this-unit-id base
    ~~~

    The GHC boot packages that are 'wired-in' cannot be shaddowed with different
    versions of the same package. Given their dependencies, the use of these
    boot packages in a build plan may limit what can be specified as an
    extra-dep.

    For example, GHC boot package `ghc-9.8.2` has a dependency on `process`. Its
    `*.conf` file identifies the dependency as `process-1.6.18.0-4fb7`. If
    package `ghc-9.8.2` is part of a build plan and a different version of
    `process` is specified as an extra-dep, during a build, Stack will identify
    that the build plan refers to two versions of `process` and warn that the
    build is likely to fail.

    Stack treats the following as the names of 'wired-in' packages: `base`,
    `dph-par`, `dph-seq`, `ghc-bignum`, `ghc-prim`, `ghc`, `integer-gmp`,
    `integer-simple`, `interactive`, `rts` and `template-haskell`.

## flags

Default: `{}`

Command line equivalent (takes precedence):
[`stack build --flag`](../../commands/build_command.md#-flag-option) option

Cabal flags can be set for each package separately. For example:

~~~yaml
flags:
  package-name:
    flag-name: true
~~~

This overrides all Cabal flag specifications (if any) for the specified packages
in the snapshot.

!!! note

    For a package included directly in the snapshot, if the Cabal flag
    specifications differ from the Cabal flag specifications (if any) in the
    snapshot, then the package will automatically be promoted to be an
    [extra-dep](#extra-deps).

!!! note

    In order to set a Cabal flag for a GHC boot package, the package must be
    specified as an [extra-dep](#extra-deps).

## drop-packages

[:octicons-tag-24: 2.1.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1)

Default: `[]`

Packages which, when present in the snapshot specified in the
[`snapshot`](#snapshot) or [`resolver`](#resolver) key, should not be included
in our project. This can be used for a few different purposes, e.g.:

* Ensure that packages you don't want used in your project cannot be used in a
  `package.yaml` file (e.g., for license reasons)
* When using a custom GHC build, avoid incompatible packages (see this
  [comment](https://github.com/commercialhaskell/stack/pull/4655#issuecomment-477954429)).

~~~yaml
drop-packages:
- buggy-package
- package-with-unacceptable-license
~~~

!!! info

    Stackage snapshots LTS Haskell 14.27 (GHC 8.6.5) and earlier, and Nightly
    2022-02-08 (GHC 8.8.2) and earlier, included directly the `Cabal` package.
    Later snapshots do not include directly that package (which is a GHC boot
    package).

    For the older Stackage snapshots, it could be handy to drop the
    snapshot-specified `Cabal` package, to avoid building that version of the
    package. For the later snapshots, there is no package version to drop.

## user-message

If present, specifies a message to be displayed every time the configuration is
loaded by Stack. It can serve as a reminder for the user to review the
configuration and make any changes if needed. The user can delete this message
if the generated configuration is acceptable.

Consecutive line ends in the message are interpreted as a single blank line.

For example, a user-message is inserted by `stack init` when it omits packages
or adds external dependencies, namely:

~~~yaml
user-message: |
  Warning (added by new or init): Some packages were found to be incompatible
  with the snapshot and have been left commented out in the packages section.

  Warning (added by new or init): Specified snapshot could not satisfy all
  dependencies. Some external packages have been added as dependencies.

  You can omit this message by removing it from the project-level configuration
  file.
~~~

## custom-preprocessor-extensions

Default: `[]`

Command line equivalent: `--customer-preprocessor-extensions` option

In order for Stack to be aware of any custom preprocessors you are using, add
their extensions here

~~~yaml
custom-preprocessor-extensions:
- erb
~~~

??? example "Use of a custom preprocessor"

    The [Ruby](https://www.ruby-lang.org/en/) programming language provides
    [`erb`](https://docs.ruby-lang.org/en/master/ERB.html) at the command line.
    `erb` provides a templating system for Ruby. The following example uses
    `erb` as a custom preprocessor.

    The example is a single-package project with a customised `Setup.hs`, which
    Stack will use to build:
    ~~~haskell
    {-# LANGUAGE CPP #-}

    module Main
      ( main
      ) where

    import           Distribution.Simple ( defaultMainWithHooks, simpleUserHooks )
    import           Distribution.Simple.PreProcess
                       ( PreProcessor (..), mkSimplePreProcessor, unsorted )
    import           Distribution.Simple.UserHooks ( UserHooks (..) )
    import           Distribution.Types.BuildInfo ( BuildInfo )
    import           Distribution.Types.ComponentLocalBuildInfo
                       ( ComponentLocalBuildInfo )
    import           Distribution.Types.LocalBuildInfo ( LocalBuildInfo )
    import           System.Process ( readCreateProcess, proc, shell )

    main :: IO ()
    main = defaultMainWithHooks simpleUserHooks
      { hookedPreProcessors = [("erb", runRuby)]
      }

    runRuby ::
         BuildInfo
      -> LocalBuildInfo
      -> ComponentLocalBuildInfo
      -> PreProcessor
    runRuby _ _ _ = PreProcessor
      { platformIndependent = True
      , ppOrdering = unsorted
      , runPreProcessor = mkSimplePreProcessor $ \erbFile fout verbosity ->
          readCreateProcess (erbProcess erbFile) "" >>= writeFile fout
      }
     where
       erbProcess erbFile =
    #if defined(mingw32_HOST_OS)
         shell $ "erb " <> erbFile
    #else
         proc "erb" [erbFile]
    #endif
    ~~~

    The example has a package description file (`package.yaml`) that specifies a
    `Custom` build type:
    ~~~yaml
    spec-version: 0.36.0
    name: my-package
    version: 0.1.0.0
    build-type: Custom

    dependencies: base

    custom-setup:
      dependencies:
      - base
      - Cabal
      - process

    library:
      source-dirs: src
      generated-exposed-modules: MyModule
    ~~~

    The example has a `src/MyModule.erb` file that will be preprocessed to
    create Haskell source code:
    ~~~text
    module MyModule where

    <% (1..5).each do |i| %>
    test<%= i %> :: Int
    test<%= i %> = <%= i %>
    <% end %>
    ~~~

    The example has a project-level configuration file (`stack.yaml`):
    ~~~yaml
    snapshot: lts-22.30
    custom-preprocessor-extensions:
    - erb
    ~~~

## extra-package-dbs

[:octicons-tag-24: 0.1.6.0](https://github.com/commercialhaskell/stack/releases/tag/v0.1.6.0)

Default: `[]`

A list of relative or absolute paths to package databases. These databases will
be added on top of GHC's global package database before the addition of other
package databases.

!!! warning

    Use of this feature may result in builds that are not reproducible, as Stack
    has no control over the contents of the extra package databases.

## curator

:octicons-beaker-24: Experimental

[:octicons-tag-24: 2.1.0.1](https://github.com/commercialhaskell/stack/releases/tag/v2.1.0.1)

Default: `{}`

Configuration intended for use only by the
[`curator` tool](https://github.com/commercialhaskell/curator), which uses Stack
to build packages. For given package names (which need not exist in the
project), Stack can be configured to ignore (skip) silently building test
suites, building benchmarks and/or creating Haddock documentation or to expect
that building test suites, building benchmarks and/or creating Haddock
documentation will fail.

For example:

~~~yaml
curator:
  skip-test:
  - my-package1
  expect-test-failure:
  - my-package2
  skip-bench:
  - my-package3
  expect-benchmark-failure:
  - my-package4
  skip-haddock:
  - my-package5
  expect-haddock-failure:
  - my-package6
~~~
