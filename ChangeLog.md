# Changelog


## Unreleased changes

Release notes:

Major changes:

* Upgrade from Cabal 2.0 to Cabal 2.2

Behavior changes:

* `stack setup` no longer uses different GHC configure options on Linux
  distributions that use GCC with PIE enabled by default.  GHC detects
  this itself since ghc-8.0.2, and Stack's attempted workaround for older
  versions caused more problems than it solved.

* `stack new` no longer initializes a project if the project template contain
   a stack.yaml file.

Other enhancements:

* A new sub command `ls` has been introduced to stack to view
  local and remote snapshots present in the system. Use `stack ls
  snapshots --help` to get more details about it.
*`list-dependencies` has been deprecated. The functionality has
  to accessed through the new `ls dependencies` interface. See
  [#3669](https://github.com/commercialhaskell/stack/issues/3669)
  for details.
* Specify User-Agent HTTP request header on every HTTP request.
  See [#3628](https://github.com/commercialhaskell/stack/issues/3628) for details.
* `stack setup` looks for GHC bindists and installations by any OS key
  that is compatible (rather than only checking a single one).   This is
  relevant on Linux where different distributions may have different
  combinations of libtinfo 5/6, ncurses 5/6, and gmp 4/5, and will allow
  simpifying the setup-info metadata YAML for future GHC releases.
* The build progress bar reports names of packages currently building.
* `stack setup --verbose` causes verbose output of GHC configure process.
  See [#3716](https://github.com/commercialhaskell/stack/issues/3716)
* Improve the error message when an `extra-dep` from a path or git reference can't be found
  See [#3808](https://github.com/commercialhaskell/stack/pull/3808)
* Nix integration is now disabled on windows even if explicitly enabled,
  since it isn't supported. See
  [#3600](https://github.com/commercialhaskell/stack/issues/3600)
* `stack build` now supports a new flag `--keep-tmp-files` to retain intermediate
  files and directories for the purpose of debugging.
  It is best used with ghc's equivalent flag,
  i.e. `stack build --keep-tmp-files --ghc-options=-keep-tmp-files`.
  See [#3857](https://github.com/commercialhaskell/stack/issues/3857)
* Improved error messages for snapshot parse exceptions

Bug fixes:



## v1.6.5

Bug fixes:
* 1.6.1 introduced a change that made some precompiled cache files use
  longer paths, sometimes causing builds to fail on windows. This has been
  fixed. See [#3649](https://github.com/commercialhaskell/stack/issues/3649)
* The script interpreter's implicit file arguments are now passed before other
  arguments. See [#3658](https://github.com/commercialhaskell/stack/issues/3658).
  In particular, this makes it possible to pass `-- +RTS ... -RTS` to specify
  RTS arguments used when running the script.
* Don't ignore the template `year` parameter in config files, and clarify the
  surrounding documentation. See
  [#2275](https://github.com/commercialhaskell/stack/issues/2275).
* Benchmarks used to be run concurrently with other benchmarks
  and build steps. This is non-ideal because CPU usage of other processes
  may interfere with benchmarks. It also prevented benchmark output from
  being displayed by default. This is now fixed. See
  [#3663](https://github.com/commercialhaskell/stack/issues/3663).
* Some unnecessary rebuilds when no files were changed are now avoided, by
  having a separate build cache for each component of a package. See
  [#3732](https://github.com/commercialhaskell/stack/issues/3732).
* Correct the behavior of promoting a package from snapshot to local
  package. This would get triggered when version bounds conflicted in
  a snapshot, which could be triggered via Hackage revisions for old
  packages. This also should allow custom snapshots to define
  conflicting versions of packages without issue. See
  [Stackage issue #3185](https://github.com/fpco/stackage/issues/3185).
* When promoting packages from snapshot to local, we were
  occassionally discarding the actual package location content and
  instead defaulting to pulling the package from the index. We now
  correctly retain this information. Note that if you were affected by
  this bug, you will likely need to delete the binary build cache
  associated with the relevant custom snapshot. See
  [#3714](https://github.com/commercialhaskell/stack/issues/3714).
* `stack ghci` now allows loading multiple packages with the same
  module name, as long as they have the same filepath. See
  [#3776](https://github.com/commercialhaskell/stack/pull/3776).
* `stack ghci` no longer always adds a dependency on `base`. It is
  now only added when there are no local targets. This allows it to
  be to load code that uses replacements for `base`. See
  [#3589](https://github.com/commercialhaskell/stack/issues/3589#issuecomment)
* `--no-rerun-tests` has been fixed. Previously, after running a test
  we were forgetting to record the result, which meant that all tests
  always ran even if they had already passed before. See
  [#3770](https://github.com/commercialhaskell/stack/pull/3770).
* Includes a patched version of `hackage-security` which fixes both
  some issues around asynchronous exception handling, and moves from
  directory locking to file locking, making the update mechanism
  resilient against SIGKILL and machine failure. See
  [hackage-security #187](https://github.com/haskell/hackage-security/issues/187)
  and [#3073](https://github.com/commercialhaskell/stack/issues/3073).
* `stack ghci` now uses correct paths for autogen files with
  [#3791](https://github.com/commercialhaskell/stack/issues/3791)


## v1.6.3.1

Hackage-only release with no user facing changes (updated to build with
newer version of hpack dependency).


## v1.6.3

Enhancements:

* In addition to supporting `.tar.gz` and `.zip` files as remote archives,
  plain `.tar` files are now accepted too. This will additionally help with
  cases where HTTP servers mistakenly set the transfer encoding to `gzip`. See
  [#3647](https://github.com/commercialhaskell/stack/issues/3647).
* Links to docs.haskellstack.org ignore Stack version patchlevel.
* Downloading Docker-compatible `stack` binary ignores Stack version patchlevel.

Bug fixes:

* For versions of Cabal before 1.24, ensure that the dependencies of
  non-buildable components are part of the build plan to work around an old
  Cabal bug. See [#3631](https://github.com/commercialhaskell/stack/issues/3631).
* Run the Cabal file checking in the `sdist` command more reliably by
  allowing the Cabal library to flatten the
  `GenericPackageDescription` itself.

## v1.6.1.1

Hackage-only release with no user facing changes (updated to build with
newer dependency versions).

## v1.6.1

Major changes:

* Complete overhaul of how snapshots are defined, the `packages` and
  `extra-deps` fields, and a number of related items. For full
  details, please see
  [the writeup on these changes](https://www.fpcomplete.com/blog/2017/07/stacks-new-extensible-snapshots). [PR #3249](https://github.com/commercialhaskell/stack/pull/3249),
  see the PR description for a number of related issues.
* Upgraded to version 2.0 of the Cabal library.

Behavior changes:

* The `--install-ghc` flag is now on by default. For example, if you
  run `stack build` in a directory requiring a GHC that you do not
  currently have, Stack will automatically download and install that
  GHC. You can explicitly set `install-ghc: false` or pass the flag
  `--no-install-ghc` to regain the previous behavior.
* `stack ghci` no longer loads modules grouped by package. This is
  always an improvement for plain ghci - it makes loading faster and
  less noisy. For intero, this has the side-effect that it will no
  longer load multiple packages that depend on TH loading relative
  paths.  TH relative paths will still work when loading a single
  package into intero. See
  [#3309](https://github.com/commercialhaskell/stack/issues/3309)
* Setting GHC options for a package via `ghc-options:` in your
  `stack.yaml` will promote it to a local package, providing for more
  consistency with flags and better reproducibility. See:
  [#849](https://github.com/commercialhaskell/stack/issues/849)
* The `package-indices` setting with Hackage no longer works with the
  `00-index.tar.gz` tarball, but must use the `01-index.tar.gz` file
  to allow revised packages to be found.
* Options passed via `--ghci-options` are now passed to the end of the
  invocation of ghci, instead of the middle.  This allows using `+RTS`
  without an accompanying `-RTS`.
* When auto-detecting `--ghc-build`, `tinfo6` is now preferred over
  `standard` if both versions of libtinfo are installed
* Addition of `stack build --copy-compiler-tool`, to allow tools like
  intero to be installed globally for a particular compiler.
  [#2643](https://github.com/commercialhaskell/stack/issues/2643)
* Stack will ask before saving hackage credentials to file. This new
  prompt can be avoided by using the `save-hackage-creds` setting. Please
  see [#2159](https://github.com/commercialhaskell/stack/issues/2159).
* The `GHCRTS` environment variable will no longer be passed through to
  every program stack runs. Instead, it will only be passed through
  commands like `exec`, `runghc`, `script`, `ghci`, etc.
  See [#3444](https://github.com/commercialhaskell/stack/issues/3444).
* `ghc-options:` for specific packages will now come after the options
  specified for all packages / particular sets of packages. See
  [#3573](https://github.com/commercialhaskell/stack/issues/3573).
* The `pvp-bounds` feature is no longer fully functional, due to some
  issues with the Cabal library's printer. See
  [#3550](https://github.com/commercialhaskell/stack/issues/3550).

Other enhancements:

* The `with-hpack` configuration option specifies an Hpack executable to use
  instead of the Hpack bundled with Stack. Please
  see [#3179](https://github.com/commercialhaskell/stack/issues/3179).
* It's now possible to skip tests and benchmarks using `--skip`
  flag
* `GitSHA1` is now `StaticSHA256` and is implemented using the `StaticSize 64 ByteString` for improved performance.
  See [#3006](https://github.com/commercialhaskell/stack/issues/3006)
* Dependencies via HTTP(S) archives have been generalized to allow
  local file path archives, as well as to support setting a
  cryptographic hash (SHA256) of the contents for better
  reproducibility.
* Allow specifying `--git-branch` when upgrading
* When running `stack upgrade` from a file which is different from the
  default executable path (e.g., on POSIX systems,
  `~/.local/bin/stack`), it will now additionally copy the new
  executable over the currently running `stack` executable. If
  permission is denied (such as in `/usr/local/bin/stack`), the user
  will be prompted to try again using `sudo`. This is intended to
  assist with the user experience when the `PATH` environment variable
  has not been properly configured, see
  [#3232](https://github.com/commercialhaskell/stack/issues/3232).
* `stack setup` for ghcjs will now install `alex` and `happy` if
  they are not present.  See
  [#3109](https://github.com/commercialhaskell/stack/issues/3232).
* Added `stack ghci --only-main` flag, to skip loading / importing
  all but main modules. See the ghci documentation page
  for further info.
* Allow GHC's colored output to show through. GHC colors output
  starting with version 8.2.1, for older GHC this does nothing.
  Sometimes GHC's heuristics would work fine even before this change,
  for example in `stack ghci`, but this override's GHC's heuristics
  when they're broken by our collecting and processing GHC's output.
* Extended the `ghc-options` field to support `$locals`, `$targets`,
  and `$everything`. See:
  [#3329](https://github.com/commercialhaskell/stack/issues/3329)
* Better error message for case that `stack ghci` file targets are
  combined with invalid package targets. See:
  [#3342](https://github.com/commercialhaskell/stack/issues/3342)
* For profiling now uses `-fprof-auto -fprof-cafs` instead of
  the deprecated `-auto-all -caf-all`. See:
  [#3360](https://github.com/commercialhaskell/stack/issues/3360)
* Better descriptions are now available for `stack upgrade --help`. See:
  [#3070](https://github.com/commercialhaskell/stack/issues/3070)
* When using Nix, nix-shell now depends always on gcc to prevent build errors
  when using the FFI. As ghc depends on gcc anyway, this doesn't increase the
  dependency footprint.
* `--cwd DIR` can now be passed to `stack exec` in order to execute the
  program in a different directory. See:
  [#3264](https://github.com/commercialhaskell/stack/issues/3264)
* Plan construction will detect if you add an executable-only package
  as a library dependency, resulting in much clearer error
  messages. See:
  [#2195](https://github.com/commercialhaskell/stack/issues/2195).
* Addition of `--ghc-options` to `stack script` to pass options directly
  to GHC. See:
  [#3454](https://github.com/commercialhaskell/stack/issues/3454)
* Add hpack `package.yaml` to build Stack itself
* Add `ignore-revision-mismatch` setting. See:
  [#3520](https://github.com/commercialhaskell/stack/issues/3520).
* Log when each individual test suite finishes. See:
  [#3552](https://github.com/commercialhaskell/stack/issues/3552).
* Avoid spurious rebuilds when using `--file-watch` by not watching files for
  executable, test and benchmark components that aren't a target. See:
  [#3483](https://github.com/commercialhaskell/stack/issues/3483).
* Stack will now try to detect the width of the running terminal
  (only on POSIX for the moment) and use that to better display
  output messages. Work is ongoing, so some messages will not
  be optimal yet. The terminal width can be overridden with the
  new `--terminal-width` command-line option (this works even on
  non-POSIX).
* Passing non local packages as targets to `stack ghci` will now
  cause them to be used as `-package` args along with package
  hiding.
* Detect when user changed .cabal file instead of package.yaml. This
  was implemented upstream in hpack. See
  [#3383](https://github.com/commercialhaskell/stack/issues/3383).
* Automatically run `autoreconf -i` as necessary when a `configure`
  script is missing. See
  [#3534](https://github.com/commercialhaskell/stack/issues/3534)
* GHC bindists can now be identified by their SHA256 checksum in addition to
  their SHA1 checksum, allowing for more security in download.
* For filesystem setup-info paths, it's no longer assumed that the
  directory is writable, instead a temp dir is used.  See
  [#3188](https://github.com/commercialhaskell/stack/issues/3188).

Bug fixes:

* `stack hoogle` correctly generates Hoogle databases. See:
  [#3362](https://github.com/commercialhaskell/stack/issues/3362)
* `stack --docker-help` is now clearer about --docker implying
   system-ghc: true, rather than both --docker and --no-docker.
* `stack haddock` now includes package names for all modules in the
   Haddock index page. See:
  [#2886](https://github.com/commercialhaskell/stack/issues/2886)
* Fixed an issue where Stack wouldn't detect missing Docker images
  properly with newer Docker versions.
  [#3171](https://github.com/commercialhaskell/stack/pull/3171)
* Previously, cabal files with just test-suite could cause build to fail
  ([#2862](https://github.com/commercialhaskell/stack/issues/2862))
* If an invalid snapshot file has been detected (usually due to
  mismatched hashes), Stack will delete the downloaded file and
  recommend either retrying or filing an issue upstream. See
  [#3319](https://github.com/commercialhaskell/stack/issues/3319).
* Modified the flag parser within Stack to match the behavior of
  Cabal's flag parser, which allows multiple sequential dashes. See
  [#3345](https://github.com/commercialhaskell/stack/issues/3345)
* Now clears the hackage index cache if it is older than the
  downloaded index.  Fixes potential issue if stack was interrupted when
  updating index.
  See [#3033](https://github.com/commercialhaskell/stack/issues/3033)
* The Stack install script now respects the `-d` option.
  See [#3366](https://github.com/commercialhaskell/stack/pull/3366).
* `stack script` can now handle relative paths to source files.
  See [#3372](https://github.com/commercialhaskell/stack/issues/3372).
* Fixes explanation of why a target is needed by the build plan, when the
  target is an extra dependency from the commandline.
  See [#3378](https://github.com/commercialhaskell/stack/issues/3378).
* Previously, if you delete a yaml file from ~/.stack/build-plan, it would
  trust the etag and not re-download.  Fixed in this version.
* Invoking `stack --docker` in parallel now correctly locks the sqlite database.
  See [#3400](https://github.com/commercialhaskell/stack/issues/3400).
* docs.haskellstack.org RTD documentation search is replaced by the mkdocs
  search. Please see
  [#3376](https://github.com/commercialhaskell/stack/issues/3376).
* `stack clean` now works with nix.  See
  [#3468](https://github.com/commercialhaskell/stack/issues/3376).
* `stack build --only-dependencies` no longer builds local project packages
  that are depended on. See
  [#3476](https://github.com/commercialhaskell/stack/issues/3476).
* Properly handle relative paths stored in the precompiled cache files. See
  [#3431](https://github.com/commercialhaskell/stack/issues/3431).
* In some cases, Cabal does not realize that it needs to reconfigure, and must
  be told to do so automatically. This would manifest as a "shadowed
  dependency" error message. We now force a reconfigure whenever a dependency is
  built, even if the package ID remained the same. See
  [#2781](https://github.com/commercialhaskell/stack/issues/2781).
* When `--pvp-bounds` is enabled for sdist or upload, internal
  dependencies could cause errors when uploaded to hackage.  This is
  fixed, see [#3290](https://github.com/commercialhaskell/stack/issues/3290)
* Fixes a bug where nonexistent hackage versions would cause stack to
  suggest the same package name, without giving version info. See
  [#3562](https://github.com/commercialhaskell/stack/issues/3562)
* Fixes a bug that has existed since 1.5.0, where
  `stack setup --upgrade-cabal` would say that Cabal is already the latest
  version, when it wasn't.
* Ensure that an `extra-dep` from a local directory is not treated as
  a `$locals` for GHC options purposes. See
  [#3574](https://github.com/commercialhaskell/stack/issues/3574).
* Building all executables only happens once instead of every
  time. See
  [#3229](https://github.com/commercialhaskell/stack/issues/3229) for
  more info.


## 1.5.1

Bug fixes:

* Stack eagerly tries to parse all cabal files related to a
  snapshot. Starting with Stackage Nightly 2017-07-31, snapshots are
  using GHC 8.2.1, and the `ghc.cabal` file implicitly referenced uses
  the (not yet supported) Cabal 2.0 file format. Future releases of
  Stack will both be less eager about cabal file parsing and support
  Cabal 2.0. This patch simply bypasses the error for invalid parsing.


## 1.5.0

Behavior changes:

* `stack profile` and `stack trace` now add their extra RTS arguments for
  benchmarks and tests to the beginning of the args, instead of the end.
  See [#2399](https://github.com/commercialhaskell/stack/issues/2399)
* Support for Git-based indices has been removed.

Other enhancements:

* `stack setup` allow to control options passed to ghcjs-boot with
  `--ghcjs-boot-options` (one word at a time) and `--[no-]ghcjs-boot-clean`
* `stack setup` now accepts a `--install-cabal VERSION` option which
  will install a specific version of the Cabal library globally.
* Updates to store-0.4.1, which has improved performance and better error
  reporting for version tags.  A side-effect of this is that all of
  stack's binary caches will be invalidated.
* `stack solver` will now warn about unexpected cabal-install versions.
  See [#3044](https://github.com/commercialhaskell/stack/issues/3044)
* Upstream packages unpacked to a temp dir are now deleted as soon as
  possible to avoid running out of space in `/tmp`.
  See [#3018](https://github.com/commercialhaskell/stack/issues/3018)
* Add short synonyms for `test-arguments` and `benchmark-arguments` options.
* Adds `STACK_WORK` environment variable, to specify work dir.
  See [#3063](https://github.com/commercialhaskell/stack/issues/3063)
* Can now use relative paths for `extra-include-dirs` and `extra-lib-dirs`.
  See [#2830](https://github.com/commercialhaskell/stack/issues/2830)
* Improved bash completion for many options, including `--ghc-options`,
  `--flag`, targets, and project executables for `exec`.
* `--haddock-arguments` is actually used now when `haddock` is invoked
  during documentation generation.
* `--[no-]haddock-hyperlink-source` flag added which allows toggling
  of sources being included in Haddock output.
  See [#3099](https://github.com/commercialhaskell/stack/issues/3099)
* `stack ghci` will now skip building all local targets, even if they have
  downstream deps, as long as it's registered in the DB.
* The pvp-bounds feature now supports adding `-revision` to the end of
  each value, e.g. `pvp-bounds: both-revision`. This means that, when
  uploading to Hackage, Stack will first upload your tarball with an
  unmodified `.cabal` file, and then upload a cabal file revision with
  the PVP bounds added. This can be useful&mdash;especially combined
  with the
  [Stackage no-revisions feature](http://www.snoyman.com/blog/2017/04/stackages-no-revisions-field)&mdash;as
  a method to ensure PVP compliance without having to proactively fix
  bounds issues for Stackage maintenance.
* Expose a `save-hackage-creds` configuration option
* On GHC <= 7.8, filters out spurious linker warnings on windows
  See [#3127](https://github.com/commercialhaskell/stack/pull/3127)
* Better error messages when creating or building packages which alias
  wired-in packages. See
  [#3172](https://github.com/commercialhaskell/stack/issues/3172).
* MinGW bin folder now is searched for dynamic libraries. See [#3126](https://github.com/commercialhaskell/stack/issues/3126)
* When using Nix, nix-shell now depends always on git to prevent runtime errors
  while fetching metadata
* The `stack unpack` command now accepts a form where an explicit
  Hackage revision hash is specified, e.g. `stack unpack
  foo-1.2.3@gitsha1:deadbeef`. Note that this should be considered
  _experimental_, Stack will likely move towards a different hash
  format in the future.
* Binary "stack upgrade" will now warn if the installed executable is not
  on the PATH or shadowed by another entry.
* Allow running tests on tarball created by sdist and upload
  [#717](https://github.com/commercialhaskell/stack/issues/717).

Bug fixes:

* Fixes case where `stack build --profile` might not cause executables /
  tests / benchmarks to be rebuilt.
  See [#2984](https://github.com/commercialhaskell/stack/issues/2984)
* `stack ghci file.hs` now loads the file even if it isn't part of
  your project.
* `stack clean --full` now works when docker is enabled.
  See [#2010](https://github.com/commercialhaskell/stack/issues/2010)
* Fixes an issue where cyclic deps can cause benchmarks or tests to be run
  before they are built.
  See [#2153](https://github.com/commercialhaskell/stack/issues/2153)
* Fixes `stack build --file-watch` in cases where a directory is removed
  See [#1838](https://github.com/commercialhaskell/stack/issues/1838)
* Fixes `stack dot` and `stack list-dependencies` to use info from the
  package database for wired-in-packages (ghc, base, etc).
  See [#3084](https://github.com/commercialhaskell/stack/issues/3084)
* Fixes `stack --docker build` when user is part of libvirt/libvirtd
  groups on Ubuntu Yakkety (16.10).
  See [#3092](https://github.com/commercialhaskell/stack/issues/3092)
* Switching a package between extra-dep and local package now forces
  rebuild (previously it wouldn't if versions were the same).
  See [#2147](https://github.com/commercialhaskell/stack/issues/2147)
* `stack upload` no longer reveals your password when you type it on
  MinTTY-based Windows shells, such as Cygwin and MSYS2.
  See [#3142](https://github.com/commercialhaskell/stack/issues/3142)
* `stack script`'s import parser will now properly parse files that
  have Windows-style line endings (CRLF)


## 1.4.0

Release notes:

* Docker images:
  [fpco/stack-full](https://hub.docker.com/r/fpco/stack-full/) and
  [fpco/stack-run](https://hub.docker.com/r/fpco/stack-run/)
  are no longer being built for LTS 8.0 and above.
  [fpco/stack-build](https://hub.docker.com/r/fpco/stack-build/)
  images continue to be built with a
  [simplified process](https://github.com/commercialhaskell/stack/tree/master/etc/dockerfiles/stack-build).
  [#624](https://github.com/commercialhaskell/stack/issues/624)

Major changes:

* A new command, `script`, has been added, intended to make the script
  interpreter workflow more reliable, easier to use, and more
  efficient. This command forces the user to provide a `--resolver`
  value, ignores all config files for more reproducible results, and
  optimizes the existing package check to make the common case of all
  packages already being present much faster. This mode does require
  that all packages be present in a snapshot, however.
  [#2805](https://github.com/commercialhaskell/stack/issues/2805)

Behavior changes:

* The default package metadata backend has been changed from Git to
  the 01-index.tar.gz file, from the hackage-security project. This is
  intended to address some download speed issues from Github for
  people in certain geographic regions. There is now full support for
  checking out specific cabal file revisions from downloaded tarballs
  as well. If you manually specify a package index with only a Git
  URL, Git will still be used. See
  [#2780](https://github.com/commercialhaskell/stack/issues/2780)
* When you provide the `--resolver` argument to the `stack unpack`
  command, any packages passed in by name only will be looked up in
  the given snapshot instead of taking the latest version. For
  example, `stack --resolver lts-7.14 unpack mtl` will get version
  2.2.1 of `mtl`, regardless of the latest version available in the
  package indices. This will also force the same cabal file revision
  to be used as is specified in the snapshot.

    Unpacking via a package identifier (e.g. `stack --resolver lts-7.14
    unpack mtl-2.2.1`) will ignore any settings in the snapshot and take
    the most recent revision.

    For backwards compatibility with tools relying on the presence of a
    `00-index.tar`, Stack will copy the `01-index.tar` file to
    `00-index.tar`. Note, however, that these files are different; most
    importantly, 00-index contains only the newest revisions of cabal
    files, while 01-index contains all versions. You may still need to
    update your tooling.
* Passing `--(no-)nix-*` options now no longer implies `--nix`, except for
  `--nix-pure`, so that the user preference whether or not to use Nix is
  honored even in the presence of options that change the Nix behavior.

Other enhancements:

* Internal cleanup: configuration types are now based much more on lenses
* `stack build` and related commands now allow the user to disable debug symbol stripping
  with new `--no-strip`, `--no-library-stripping`, and `--no-executable-shipping` flags,
  closing [#877](https://github.com/commercialhaskell/stack/issues/877).
  Also turned error message for missing targets more readable ([#2384](https://github.com/commercialhaskell/stack/issues/2384))
* `stack haddock` now shows index.html paths when documentation is already up to
  date. Resolved [#781](https://github.com/commercialhaskell/stack/issues/781)
* Respects the `custom-setup` field introduced in Cabal 1.24. This
  supercedes any `explicit-setup-deps` settings in your `stack.yaml`
  and trusts the package's `.cabal` file to explicitly state all its
  dependencies.
* If system package installation fails, `get-stack.sh` will fail as well. Also
  shows warning suggesting to run `apt-get update` or similar, depending on the
  OS.
  ([#2898](https://github.com/commercialhaskell/stack/issues/2898))
* When `stack ghci` is run with a config with no packages (e.g. global project),
  it will now look for source files in the current work dir.
  ([#2878](https://github.com/commercialhaskell/stack/issues/2878))
* Bump to hpack 0.17.0 to allow `custom-setup` and `!include "..."` in `package.yaml`.
* The script interpreter will now output error logging.  In particular,
  this means it will output info about plan construction errors.
  ([#2879](https://github.com/commercialhaskell/stack/issues/2879))
* `stack ghci` now takes `--flag` and `--ghc-options` again (inadvertently
  removed in 1.3.0).
  ([#2986](https://github.com/commercialhaskell/stack/issues/2986))
* `stack exec` now takes `--rts-options` which passes the given arguments inside of
  `+RTS ... args .. -RTS` to the executable. This works around stack itself consuming
  the RTS flags on Windows. ([#2640](https://github.com/commercialhaskell/stack/issues/2640))
* Upgraded `http-client-tls` version, which now offers support for the
  `socks5://` and `socks5h://` values in the `http_proxy` and `https_proxy`
  environment variables.

Bug fixes:

* Bump to hpack 0.16.0 to avoid character encoding issues when reading and
  writing on non-UTF8 systems.
* `stack ghci` will no longer ignore hsSourceDirs that contain `..`. ([#2895](https://github.com/commercialhaskell/stack/issues/2895))
* `stack list-dependencies --license` now works for wired-in-packages,
  like base. ([#2871](https://github.com/commercialhaskell/stack/issues/2871))
* `stack setup` now correctly indicates when it uses system ghc
  ([#2963](https://github.com/commercialhaskell/stack/issues/2963))
* Fix to `stack config set`, in 1.3.2 it always applied to
  the global project.
  ([#2709](https://github.com/commercialhaskell/stack/issues/2709))
* Previously, cabal files without exe or lib would fail on the "copy" step.
  ([#2862](https://github.com/commercialhaskell/stack/issues/2862))
* `stack upgrade --git` now works properly.  Workaround for affected
  versions (>= 1.3.0) is to instead run `stack upgrade --git --source-only`.
  ([#2977](https://github.com/commercialhaskell/stack/issues/2977))
* Added support for GHC 8's slightly different warning format for
  dumping warnings from logs.
* Work around a bug in Cabal/GHC in which package IDs are not unique
  for different source code, leading to Stack not always rebuilding
  packages depending on local packages which have
  changed. ([#2904](https://github.com/commercialhaskell/stack/issues/2904))

## 1.3.2

Bug fixes:

* `stack config set` can now be used without a compiler installed
  [#2852](https://github.com/commercialhaskell/stack/issues/2852).
* `get-stack.sh` now installs correct binary on ARM for generic linux and raspbian,
  closing [#2856](https://github.com/commercialhaskell/stack/issues/2856).
* Correct the testing of whether a package database exists by checking
  for the `package.cache` file itself instead of the containing
  directory.
* Revert a change in the previous release which made it impossible to
  set local extra-dep packages as targets. This was overkill; we
  really only wanted to disable their test suites, which was already
  handled by a later
  patch. [#2849](https://github.com/commercialhaskell/stack/issues/2849)
* `stack new` always treats templates as being UTF-8 encoding,
  ignoring locale settings on a local machine. See
  [Yesod mailing list discussion](https://groups.google.com/d/msg/yesodweb/ZyWLsJOtY0c/aejf9E7rCAAJ)

## 1.3.0

Release notes:

* For the _next_ stack release after this one, we are planning
  changes to our Linux releases, including dropping our Ubuntu,
  Debian, CentOS, and Fedora package repositories and switching to
  statically linked binaries. See
  [#2534](https://github.com/commercialhaskell/stack/issues/2534).
  Note that upgrading without a package manager has gotten easier
  with new binary upgrade support in `stack upgrade` (see the Major
  Changes section below for more information). In addition, the
  get.haskellstack.org script no longer installs from Ubuntu,
  Debian, CentOS, or Fedora package repositories. Instead it places
  a generic binary in /usr/local/bin.

Major changes:

* Stack will now always use its own GHC installation, even when a suitable GHC
  installation is available on the PATH. To get the old behaviour, use
  the `--system-ghc` flag or run `stack config set system-ghc --global true`.
  Docker- and Nix-enabled projects continue to use the GHC installations
  in their environment by default.

    NB: Scripts that previously used stack in combination with a system GHC
    installation should now include a `stack setup` line or use the `--install-ghc`
    flag.
    [#2221](https://github.com/commercialhaskell/stack/issues/2221)

* `stack ghci` now defaults to skipping the build of target packages, because
  support has been added for invoking "initial build steps", which create
  autogen files and run preprocessors. The `--no-build` flag is now deprecated
  because it should no longer be necessary. See
  [#1364](https://github.com/commercialhaskell/stack/issues/1364)

* Stack is now capable of doing binary upgrades instead of always
  recompiling a new version from source. Running `stack upgrade` will
  now default to downloading a binary version of Stack from the most
  recent release, if one is available. See `stack upgrade --help` for
  more options.
  [#1238](https://github.com/commercialhaskell/stack/issues/1238)

Behavior changes:

* Passing `--resolver X` with a Stack command which forces creation of a global
  project config, will pass resolver X into the initial config.
  See [#2579](https://github.com/commercialhaskell/stack/issues/2229).

* Switch the "Run from outside project" messages to debug-level, to
  avoid spamming users in the normal case of non-project usage

* If a remote package is specified (such as a Git repo) without an explicit
  `extra-dep` setting, a warning is given to the user to provide one
  explicitly.

Other enhancements:

* `stack haddock` now supports `--haddock-internal`. See
  [#2229](https://github.com/commercialhaskell/stack/issues/2229)
* Add support for `system-ghc` and `install-ghc` fields to `stack config set` command.
* Add `ghc-build` option to override autodetected GHC build to use (e.g. gmp4,
  tinfo6, nopie) on Linux.
* `stack setup` detects systems where gcc enables PIE by default (such as Ubuntu
  16.10 and Hardened Gentoo) and adjusts the GHC `configure` options accordingly.
  [#2542](https://github.com/commercialhaskell/stack/issues/2542)
* Upload to Hackage with HTTP digest instead of HTTP basic.
* Make `stack list-dependencies` understand all of the `stack dot` options too.
* Add the ability for `stack list-dependencies` to list dependency licenses by
  passing the `--license` flag.
* Dump logs that contain warnings for any local non-dependency packages
  [#2545](https://github.com/commercialhaskell/stack/issues/2545)
* Add the `dump-logs` config option and `--dump-logs` command line
  option to get full build output on the
  console. [#426](https://github.com/commercialhaskell/stack/issues/426)
* Add the `--open` option to "stack hpc report" command, causing the report to
  be opened in the browser.
* The `stack config set` command now accepts a `--global` flag for suitable fields
  which causes it to modify the global user configuration (`~/.stack/config.yaml`)
  instead of the project configuration.
  [#2675](https://github.com/commercialhaskell/stack/pull/2675)
* Information on the latest available snapshots is now downloaded from S3 instead of
  stackage.org, increasing reliability in case of stackage.org outages.
  [#2653](https://github.com/commercialhaskell/stack/pull/2653)
* `stack dot` and `stack list-dependencies` now take targets and flags.
  [#1919](https://github.com/commercialhaskell/stack/issues/1919)
* Deprecate `stack setup --stack-setup-yaml` for `--setup-info-yaml` based
  on discussion in [#2647](https://github.com/commercialhaskell/stack/issues/2647).
* The `--main-is` flag for GHCI now implies the TARGET, fixing
  [#1845](https://github.com/commercialhaskell/stack/issues/1845).
* `stack ghci` no longer takes all build options, as many weren't useful
  [#2199](https://github.com/commercialhaskell/stack/issues/2199)
* `--no-time-in-log` option, to make verbose logs more diffable
  [#2727](https://github.com/commercialhaskell/stack/issues/2727)
* `--color` option added to override auto-detection of ANSI support
  [#2725](https://github.com/commercialhaskell/stack/issues/2725)
* Missing extra-deps are now warned about, adding a degree of typo detection
  [#1521](https://github.com/commercialhaskell/stack/issues/1521)
* No longer warns about missing build-tools if they are on the PATH.
  [#2235](https://github.com/commercialhaskell/stack/issues/2235)
* Replace enclosed-exceptions with safe-exceptions.
  [#2768](https://github.com/commercialhaskell/stack/issues/2768)
* The install location for GHC and other programs can now be configured with the
  `local-programs-path` option in `config.yaml`.
  [#1644](https://github.com/commercialhaskell/stack/issues/1644)
* Added option to add nix dependencies as nix GC roots
* Proper pid 1 (init) process for `stack exec` with Docker
* Dump build logs if they contain warnings.
  [#2545](https://github.com/commercialhaskell/stack/issues/2545)
* Docker: redirect stdout of `docker pull` to stderr so that
  it will not interfere with output of other commands.
* Nix & docker can be activated at the same time, in order to run stack in a nix-shell
  in a container, preferably from an image already containing the nix dependencies
  in its /nix/store
* Stack/nix: Dependencies can be added as nix GC roots, so they are not removed
  when running `nix-collect-garbage`

Bug fixes:

* Fixed a gnarly bug where programs and package tarballs sometimes have
  corrupted downloads. See
  [#2657](https://github.com/commercialhaskell/stack/issues/2568).
* Add proper support for non-ASCII characters in file paths for the `sdist` command.
  See [#2549](https://github.com/commercialhaskell/stack/issues/2549)
* Never treat `extra-dep` local packages as targets. This ensures
  things like test suites are not run for these packages, and that
  build output is not hidden due to their presence.
* Fix a resource leak in `sinkProcessStderrStdout` which could affect
  much of the codebase, in particular copying precompiled
  packages. [#1979](https://github.com/commercialhaskell/stack/issues/1979)
* Docker: ensure that interrupted extraction process does not cause corrupt file
  when downloading a Docker-compatible Stack executable
  [#2568](https://github.com/commercialhaskell/stack/issues/2568)
* Fixed running `stack hpc report` on package targets.
  [#2664](https://github.com/commercialhaskell/stack/issues/2664)
* Fix a long-standing performance regression where stack would parse the .dump-hi
  files of the library components of local packages twice.
  [#2658](https://github.com/commercialhaskell/stack/pull/2658)
* Fixed a regression in "stack ghci --no-load", where it would prompt for a main
  module to load. [#2603](https://github.com/commercialhaskell/stack/pull/2603)
* Build Setup.hs files with the threaded RTS, mirroring the behavior of
  cabal-install and enabling more complex build systems in those files.
* Fixed a bug in passing along `--ghc-options` to ghcjs.  They were being
  provided as `--ghc-options` to Cabal, when it needs to be `--ghcjs-options`.
  [#2714](https://github.com/commercialhaskell/stack/issues/2714)
* Launch Docker from the project root regardless of the working
  directory Stack is invoked from. This means paths relative to the project root
  (e.g. environment files) can be specified in `stack.yaml`'s docker `run-args`.
* `stack setup --reinstall` now behaves as expected.
  [#2554](https://github.com/commercialhaskell/stack/issues/2554)

## 1.2.0

Release notes:

* On many Un*x systems, Stack can now be installed with a simple
  one-liner:

        wget -qO- https://get.haskellstack.org/ | sh

* The fix for
  [#2175](https://github.com/commercialhaskell/stack/issues/2175)
  entails that stack must perform a full clone of a large Git repo of
  Hackage meta-information. The total download size is about 200 MB.
  Please be aware of this when upgrading your stack installation.

* If you use Mac OS X, you may want to delay upgrading to macOS Sierra as there
  are reports of GHC panics when building some packages (including Stack
  itself). See [#2577](https://github.com/commercialhaskell/stack/issues/2577)

* This version of Stack does not build on ARM or PowerPC systems (see
  [store#37](https://github.com/fpco/store/issues/37)).  Please stay with
  version 1.1.2 for now on those architectures.  This will be rectified soon!

* We are now releasing a
  [statically linked Stack binary for 64-bit Linux](https://www.stackage.org/stack/linux-x86_64-static).
  Please try it and let us know if you run into any trouble on your platform.

* We are planning some changes to our Linux releases, including dropping our
  Ubuntu, Debian, CentOS, and Fedora package repositories and switching to
  statically linked binaries.  We would value your feedback in
  [#2534](https://github.com/commercialhaskell/stack/issues/2534).

Major changes:

* Add `stack hoogle` command.
  [#55](https://github.com/commercialhaskell/stack/issues/55)
* Support for absolute file path in `url` field of `setup-info` or `--ghc-bindist`
* Add support for rendering GHCi scripts targeting different GHCi like
  applications
  [#2457](https://github.com/commercialhaskell/stack/pull/2457)

Behavior changes:

* Remove `stack ide start` and `stack ide load-targets` commands.
  [#2178](https://github.com/commercialhaskell/stack/issues/2178)
* Support .buildinfo files in `stack ghci`.
  [#2242](https://github.com/commercialhaskell/stack/pull/2242)
* Support -ferror-spans syntax in GHC error messages.
* Avoid unpacking ghc to `/tmp`
  [#996](https://github.com/commercialhaskell/stack/issues/996)
* The Linux `gmp4` GHC bindist is no longer considered a full-fledged GHC
  variant and can no longer be specified using the `ghc-variant` option,
  and instead is treated more like a slightly different platform.

Other enhancements:

* Use the `store` package for binary serialization of most caches.
* Only require minor version match for Docker stack exe.
  This way, we can make patch releases for version bounds and similar
  build issues without needing to upload new binaries for Docker.
* Stack/Nix: Passes the right ghc derivation as an argument to the `shell.nix` when a
  custom `shell.nix` is used
  See [#2243](https://github.com/commercialhaskell/stack/issues/2243)
* Stack/Nix: Sets `LD_LIBRARY_PATH` so packages using C libs for Template Haskell can work
  (See _e.g._ [this HaskellR issue](https://github.com/tweag/HaskellR/issues/253))
* Parse CLI arguments and configuration files into less permissive types,
  improving error messages for bad inputs.
  [#2267](https://github.com/commercialhaskell/stack/issues/2267)
* Add the ability to explicitly specify a gcc executable.
  [#593](https://github.com/commercialhaskell/stack/issues/593)
* Nix: No longer uses LTS mirroring in nixpkgs. Gives to nix-shell a derivation
  like `haskell.compiler.ghc801`
  See [#2259](https://github.com/commercialhaskell/stack/issues/2259)
* Perform some subprocesses during setup concurrently, slightly speeding up most
  commands. [#2346](https://github.com/commercialhaskell/stack/pull/2346)
* `stack setup` no longer unpacks to the system temp dir on posix systems.
  [#996](https://github.com/commercialhaskell/stack/issues/996)
* `stack setup` detects libtinfo6 and ncurses6 and can download alternate GHC
  bindists [#257](https://github.com/commercialhaskell/stack/issues/257)
  [#2302](https://github.com/commercialhaskell/stack/issues/2302).
* `stack setup` detects Linux ARMv7 downloads appropriate GHC bindist
  [#2103](https://github.com/commercialhaskell/stack/issues/2103)
* Custom `stack` binaries list dependency versions in output for `--version`.
  See [#2222](https://github.com/commercialhaskell/stack/issues/2222)
  and [#2450](https://github.com/commercialhaskell/stack/issues/2450).
* Use a pretty printer to output dependency resolution errors.
  [#1912](https://github.com/commercialhaskell/stack/issues/1912)
* Remove the `--os` flag
  [#2227](https://github.com/commercialhaskell/stack/issues/2227)
* Add 'netbase' and 'ca-certificates' as dependency for .deb packages.
  [#2293](https://github.com/commercialhaskell/stack/issues/2293).
* Add `stack ide targets` command.
* Enhance debug logging with subprocess timings.
* Pretty-print YAML parse errors
  [#2374](https://github.com/commercialhaskell/stack/issues/2374)
* Clarify confusing `stack setup` output
  [#2314](https://github.com/commercialhaskell/stack/issues/2314)
* Delete `Stack.Types` multimodule to improve build times
  [#2405](https://github.com/commercialhaskell/stack/issues/2405)
* Remove spurious newlines in build logs
  [#2418](https://github.com/commercialhaskell/stack/issues/2418)
* Interpreter: Provide a way to hide implicit packages
  [#1208](https://github.com/commercialhaskell/stack/issues/1208)
* Check executability in exec lookup
  [#2489](https://github.com/commercialhaskell/stack/issues/2489)

Bug fixes:

* Fix cabal warning about use of a deprecated cabal flag
  [#2350](https://github.com/commercialhaskell/stack/issues/2350)
* Support most executable extensions on Windows
  [#2225](https://github.com/commercialhaskell/stack/issues/2225)
* Detect resolver change in `stack solver`
  [#2252](https://github.com/commercialhaskell/stack/issues/2252)
* Fix a bug in docker image creation where the wrong base image was
  selected
  [#2376](https://github.com/commercialhaskell/stack/issues/2376)
* Ignore special entries when unpacking tarballs
  [#2361](https://github.com/commercialhaskell/stack/issues/2361)
* Fixes src directory pollution of `style.css` and `highlight.js` with GHC 8's
  haddock [#2429](https://github.com/commercialhaskell/stack/issues/2429)
* Handle filepaths with spaces in `stack ghci`
  [#2266](https://github.com/commercialhaskell/stack/issues/2266)
* Apply ghc-options to snapshot packages
  [#2289](https://github.com/commercialhaskell/stack/issues/2289)
* stack sdist: Fix timestamp in tarball
  [#2394](https://github.com/commercialhaskell/stack/pull/2394)
* Allow global Stack arguments with a script
  [#2316](https://github.com/commercialhaskell/stack/issues/2316)
* Inconsistency between ToJSON and FromJSON instances of PackageLocation
  [#2412](https://github.com/commercialhaskell/stack/pull/2412)
* Perform Unicode normalization on filepaths
  [#1810](https://github.com/commercialhaskell/stack/issues/1810)
* Solver: always keep ghc wired-in as hard constraints
  [#2453](https://github.com/commercialhaskell/stack/issues/2453)
* Support OpenBSD's tar where possible, require GNU tar for xz support
  [#2283](https://github.com/commercialhaskell/stack/issues/2283)
* Fix using --coverage with Cabal-1.24
  [#2424](https://github.com/commercialhaskell/stack/issues/2424)
* When marking exe installed, remove old version
  [#2373](https://github.com/commercialhaskell/stack/issues/2373)
* Stop truncating all-cabal-hashes git repo
  [#2175](https://github.com/commercialhaskell/stack/issues/2175)
* Handle non-ASCII filenames on Windows
  [#2491](https://github.com/commercialhaskell/stack/issues/2491)
* Avoid using multiple versions of a package in script interpreter
  by passing package-id to ghc/runghc
  [#1957](https://github.com/commercialhaskell/stack/issues/1957)
* Only pre-load compiler version when using nix integration
  [#2459](https://github.com/commercialhaskell/stack/issues/2459)
* Solver: parse cabal errors also on Windows
  [#2502](https://github.com/commercialhaskell/stack/issues/2502)
* Allow exec and ghci commands in interpreter mode.
  Scripts can now automatically open in the repl by using `exec ghci`
  instead of `runghc` in the shebang command.
  [#2510](https://github.com/commercialhaskell/stack/issues/2510)
* Now consider a package to be dirty when an extra-source-file is changed.
  See [#2040](https://github.com/commercialhaskell/stack/issues/2040)

## 1.1.2

Release notes:

* Official FreeBSD binaries are
  [now available](http://docs.haskellstack.org/en/stable/install_and_upgrade/#freebsd)
  [#1253](https://github.com/commercialhaskell/stack/issues/1253).

Major changes:

* Extensible custom snapshots implemented. These allow you to define snapshots
which extend other snapshots. See
[#863](https://github.com/commercialhaskell/stack/issues/863). Local file custom
snapshots can now be safely updated without changing their name.  Remote custom
snapshots should still be treated as immutable.

Behavior changes:

* `stack path --compiler` was added in the last release, to yield a path to the
  compiler. Unfortunately, `--compiler` is a global option that is useful to use
  with `stack path`. The same functionality is now provided by `stack path
  --compiler-exe`. See
  [#2123](https://github.com/commercialhaskell/stack/issues/2123)
* For packages specified in terms of a git or hg repo, the hash used in the
  location has changed.  This means that existing downloads from older stack
  versions won't be used.  This is a side-effect of the fix to
  [#2133](https://github.com/commercialhaskell/stack/issues/2133)
* `stack upgrade` no longer pays attention to local stack.yaml files, just the
  global config and CLI options.
  [#1392](https://github.com/commercialhaskell/stack/issues/1392)
* `stack ghci` now uses `:add` instead of `:load`, making it potentially work
  better with user scripts. See
  [#1888](https://github.com/commercialhaskell/stack/issues/1888)

Other enhancements:

* Grab Cabal files via Git SHA to avoid regressions from Hackage revisions
  [#2070](https://github.com/commercialhaskell/stack/pull/2070)
* Custom snapshots now support `ghc-options`.
* Package git repos are now re-used rather than re-cloned. See
  [#1620](https://github.com/commercialhaskell/stack/issues/1620)
* `DESTDIR` is filtered from environment when installing GHC. See
  [#1460](https://github.com/commercialhaskell/stack/issues/1460)
* `stack haddock` now supports `--hadock-arguments`. See
  [#2144](https://github.com/commercialhaskell/stack/issues/2144)
* Signing: warn if GPG_TTY is not set as per `man gpg-agent`

Bug fixes:

* Now ignore project config when doing `stack init` or `stack new`. See
  [#2110](https://github.com/commercialhaskell/stack/issues/2110)
* Packages specified by git repo can now have submodules. See
  [#2133](https://github.com/commercialhaskell/stack/issues/2133)
* Fix of hackage index fetch retry. See re-opening of
  [#1418](https://github.com/commercialhaskell/stack/issues/1418#issuecomment-217633843)
* HPack now picks up changes to filesystem other than package.yaml.  See
  [#2051](https://github.com/commercialhaskell/stack/issues/2051)
* "stack solver" no longer suggests --omit-packages. See
  [#2031](https://github.com/commercialhaskell/stack/issues/2031)
* Fixed an issue with building Cabal's Setup.hs. See
  [#1356](https://github.com/commercialhaskell/stack/issues/1356)
* Package dirtiness now pays attention to deleted files. See
  [#1841](https://github.com/commercialhaskell/stack/issues/1841)
* `stack ghci` now uses `extra-lib-dirs` and `extra-include-dirs`. See
  [#1656](https://github.com/commercialhaskell/stack/issues/1656)
* Relative paths outside of source dir added via `qAddDependentFile` are now
  checked for dirtiness. See
  [#1982](https://github.com/commercialhaskell/stack/issues/1982)
* Signing: always use `--with-fingerprints`

## 1.1.0

Release notes:

* Added Ubuntu 16.04 LTS (xenial) Apt repo.
* No longer uploading new versions to Fedora 21 repo.

Behavior changes:

* Snapshot packages are no longer built with executable profiling. See
  [#1179](https://github.com/commercialhaskell/stack/issues/1179).
* `stack init` now ignores symlinks when searching for cabal files. It also now
  ignores any directory that begins with `.` (as well as `dist` dirs) - before
  it would only ignore `.git`, `.stack-work`, and `dist`.
* The stack executable is no longer built with `-rtsopts`.  Before, when
  `-rtsopts` was enabled, stack would process `+RTS` options even when intended
  for some other program, such as when used with `stack exec -- prog +RTS`.
  See [#2022](https://github.com/commercialhaskell/stack/issues/2022).
* The `stack path --ghc-paths` option is deprecated and renamed to `--programs`.
  `--compiler` is added, which points directly at the compiler used in
  the current project.  `--compiler-bin` points to the compiler's bin dir.
* For consistency with the `$STACK_ROOT` environment variable, the
  `stack path --global-stack-root` flag and the `global-stack-root` field
  in the output of `stack path` are being deprecated and replaced with the
  `stack-root` flag and output field.
  Additionally, the stack root can now be specified via the
  `--stack-root` command-line flag. See
  [#1148](https://github.com/commercialhaskell/stack/issues/1148).
* `stack sig` GPG-related sub-commands were removed (folded into `upload` and
  `sdist`)
* GPG signing of packages while uploading to Hackage is now the default. Use
  `upload --no-signature` if you would rather not contribute your package
  signature. If you don't yet have a GPG keyset, read this
  [blog post on GPG keys](https://fpcomplete.com/blog/2016/05/stack-security-gnupg-keys).
  We can add a stack.yaml config setting to disable signing if some people
  desire it. We hope that people will sign. Later we will be adding GPG
  signature verification options.
* `stack build pkg-1.2.3` will now build even if the snapshot has a different
  package version - it is treated as an extra-dep. `stack build local-pkg-1.2.3`
  is an error even if the version number matches the local package
  [#2028](https://github.com/commercialhaskell/stack/issues/2028).
* Having a `nix:` section no longer implies enabling nix build. This allows the
  user to globally configure whether nix is used (unless the project overrides
  the default explicitly). See
  [#1924](https://github.com/commercialhaskell/stack/issues/1924).
* Remove deprecated valid-wanted field.
* Docker: mount home directory in container [#1949](https://github.com/commercialhaskell/stack/issues/1949).
* Deprecate `--local-bin-path` instead `--local-bin`.
* `stack image`: allow absolute source paths for `add`.

Other enhancements:

* `stack haddock --open [PACKAGE]` opens the local haddocks in the browser.
* Fix too much rebuilding when enabling/disabling profiling flags.
* `stack build pkg-1.0` will now build `pkg-1.0` even if the snapshot specifies
  a different version (it introduces a temporary extra-dep)
* Experimental support for `--split-objs` added
  [#1284](https://github.com/commercialhaskell/stack/issues/1284).
* `git` packages with submodules are supported by passing the `--recursive`
  flag to `git clone`.
* When using [hpack](https://github.com/sol/hpack), only regenerate cabal files
  when hpack files change.
* hpack files can now be used in templates
* `stack ghci` now runs ghci as a separate process
  [#1306](https://github.com/commercialhaskell/stack/issues/1306)
* Retry when downloading snapshots and package indices
* Many build options are configurable now in `stack.yaml`:
```
  build:
    library-profiling: true
    executable-profiling: true
    haddock: true
    haddock-deps: true
    copy-bins: true
    prefetch: true
    force-dirty: true
    keep-going: true
    test: true
    test-arguments:
      rerun-tests: true
      additional-args: ['-fprof']
      coverage: true
      no-run-tests: true
    bench: true
    benchmark-opts:
      benchmark-arguments: -O2
      no-run-benchmarks: true
    reconfigure: true
    cabal-verbose: true
```
* A number of URLs are now configurable, useful for firewalls. See
  [#1794](https://github.com/commercialhaskell/stack/issues/1884).
* Suggest causes when executables are missing.
* Allow `--omit-packages` even without `--solver`.
* Improve the generated stack.yaml.
* Improve ghci results after :load Main module collision with main file path.
* Only load the hackage index if necessary
  [#1883](https://github.com/commercialhaskell/stack/issues/1883), [#1892](https://github.com/commercialhaskell/stack/issues/1892).
* init: allow local packages to be deps of deps
  [#1965](https://github.com/commercialhaskell/stack/issues/1965).
* Always use full fingerprints from GPG
  [#1952](https://github.com/commercialhaskell/stack/issues/1952).
* Default to using `gpg2` and fall back to `gpg`
  [#1976](https://github.com/commercialhaskell/stack/issues/1976).
* Add a flag for --verbosity silent.
* Add `haddock --open` flag [#1396](https://github.com/commercialhaskell/stack/issues/1396).

Bug fixes:

* Package tarballs would fail to unpack.
  [#1884](https://github.com/commercialhaskell/stack/issues/1884).
* Fixed errant warnings about missing modules, after deleted and removed from
  cabal file [#921](https://github.com/commercialhaskell/stack/issues/921)
  [#1805](https://github.com/commercialhaskell/stack/issues/1805).
* Now considers a package to dirty when the hpack file is changed
  [#1819](https://github.com/commercialhaskell/stack/issues/1819).
* Nix: cancelling a stack build now exits properly rather than dropping into a
  nix-shell [#1778](https://github.com/commercialhaskell/stack/issues/1778).
* `allow-newer: true` now causes `--exact-configuration` to be passed to Cabal.
  See [#1579](https://github.com/commercialhaskell/stack/issues/1579).
* `stack solver` no longer fails with `InvalidRelFile` for relative package
  paths including `..`. See
  [#1954](https://github.com/commercialhaskell/stack/issues/1954).
* Ignore emacs lock files when finding .cabal
  [#1897](https://github.com/commercialhaskell/stack/issues/1897).
* Use lenient UTF-8 decode for build output
  [#1945](https://github.com/commercialhaskell/stack/issues/1945).
* Clear index cache whenever index updated
  [#1962](https://github.com/commercialhaskell/stack/issues/1962).
* Fix: Building a container image drops a .stack-work dir in the current working
  (sub)directory
  [#1975](https://github.com/commercialhaskell/stack/issues/1975).
* Fix: Rebuilding when disabling profiling
  [#2023](https://github.com/commercialhaskell/stack/issues/2023).

## 1.0.4.3

Bug fixes:

* Don't delete contents of ~/.ssh when using `stack clean --full` with Docker
  enabled [#2000](https://github.com/commercialhaskell/stack/issues/2000)

## 1.0.4.2

Build with path-io-1.0.0. There are no changes in behaviour from 1.0.4,
so no binaries are released for this version.

## 1.0.4.1

Fixes build with aeson-0.11.0.0. There are no changes in behaviour from 1.0.4,
so no binaries are released for this version.

## 1.0.4

Major changes:

* Some notable changes in `stack init`:
    * Overall it should now be able to initialize almost all existing cabal
      packages out of the box as long as the package itself is consistently
      defined.
    * Choose the best possible snapshot and add extra dependencies on top
      of a snapshot resolver rather than a compiler resolver -
      [#1583](https://github.com/commercialhaskell/stack/pull/1583)
    * Automatically omit a package (`--omit-packages`) when it is compiler
      incompatible or when there are packages with conflicting dependency
      requirements - [#1674](https://github.com/commercialhaskell/stack/pull/1674).
    * Some more changes for a better user experience. Please refer to
      the doc guide for details.
* Add support for hpack, alternative package description format
  [#1679](https://github.com/commercialhaskell/stack/issues/1679)

Other enhancements:

* Docker: pass ~/.ssh and SSH auth socket into container, so that git repos
  work [#1358](https://github.com/commercialhaskell/stack/issues/1358).
* Docker: strip suffix from docker --version.
  [#1653](https://github.com/commercialhaskell/stack/issues/1653)
* Docker: pass USER and PWD environment variables into container.
* On each run, stack will test the stack root directory (~/.stack), and the
  project and package work directories (.stack-work) for whether they are
  owned by the current user and abort if they are not. This precaution can
  be disabled with the `--allow-different-user` flag or `allow-different-user`
  option in the global config (~/.stack/config.yaml).
  [#471](https://github.com/commercialhaskell/stack/issues/471)
* Added `stack clean --full` option for full working dir cleanup.
* YAML config: support Zip archives.
* Redownload build plan if parsing fails
  [#1702](https://github.com/commercialhaskell/stack/issues/1702).
* Give mustache templates access to a 'year' tag
  [#1716](https://github.com/commercialhaskell/stack/pull/1716).
* Have "stack ghci" warn about module name aliasing.
* Add "stack ghci --load-local-deps".
* Build Setup.hs with -rtsopts
  [#1687](https://github.com/commercialhaskell/stack/issues/1687).
* `stack init` accepts a list of directories.
* Add flag infos to DependencyPlanFailures (for better error output in case of
  flags) [#713](https://github.com/commercialhaskell/stack/issues/713)
* `stack new --bare` complains for overwrites, and add `--force` option
  [#1597](https://github.com/commercialhaskell/stack/issues/1597).

Bug fixes:

* Previously, `stack ghci` would fail with `cannot satisfy -package-id` when the
  implicit build step changes the package key of some dependency.
* Fix: Building with ghcjs: "ghc-pkg: Prelude.chr: bad argument: 2980338"
  [#1665](https://github.com/commercialhaskell/stack/issues/1665).
* Fix running test / bench with `--profile` / `--trace`.
* Fix: build progress counter is no longer visible
  [#1685](https://github.com/commercialhaskell/stack/issues/1685).
* Use "-RTS" w/ profiling to allow extra args
  [#1772](https://github.com/commercialhaskell/stack/issues/1772).
* Fix withUnpackedTarball7z to find name of srcDir after unpacking
  (fixes `stack setup` fails for ghcjs project on windows)
  [#1774](https://github.com/commercialhaskell/stack/issues/1774).
* Add space before auto-generated bench opts (makes profiling options work
  uniformly for applications and benchmark suites)
  [#1771](https://github.com/commercialhaskell/stack/issues/1771).
* Don't try to find plugin if it resembles flag.
* Setup.hs changes cause package dirtiness
  [#1711](https://github.com/commercialhaskell/stack/issues/1711).
* Send "stack templates" output to stdout
  [#1792](https://github.com/commercialhaskell/stack/issues/1792).

## 1.0.2

Release notes:

- Arch Linux: Stack has been adopted into the
  [official community repository](https://www.archlinux.org/packages/community/x86_64/stack/),
  so we will no longer be updating the AUR with new versions. See the
  [install/upgrade guide](http://docs.haskellstack.org/en/stable/install_and_upgrade/#arch-linux)
  for current download instructions.

Major changes:

- `stack init` and `solver` overhaul
  [#1583](https://github.com/commercialhaskell/stack/pull/1583)

Other enhancements:

- Disable locale/codepage hacks when GHC >=7.10.3
  [#1552](https://github.com/commercialhaskell/stack/issues/1552)
- Specify multiple images to build for `stack image container`
  [docs](http://docs.haskellstack.org/en/stable/yaml_configuration/#image)
- Specify which executables to include in images for `stack image container`
  [docs](http://docs.haskellstack.org/en/stable/yaml_configuration/#image)
- Docker: pass supplementary groups and umask into container
- If git fetch fails wipe the directory and try again from scratch
  [#1418](https://github.com/commercialhaskell/stack/issues/1418)
- Warn if newly installed executables won't be available on the PATH
  [#1362](https://github.com/commercialhaskell/stack/issues/1362)
- stack.yaml: for `stack image container`, specify multiple images to generate,
  and which executables should be added to those images
- GHCI: add interactive Main selection
  [#1068](https://github.com/commercialhaskell/stack/issues/1068)
- Care less about the particular name of a GHCJS sdist folder
  [#1622](https://github.com/commercialhaskell/stack/issues/1622)
- Unified Enable/disable help messaging
  [#1613](https://github.com/commercialhaskell/stack/issues/1613)

Bug fixes:

- Don't share precompiled packages between GHC/platform variants and Docker
  [#1551](https://github.com/commercialhaskell/stack/issues/1551)
- Properly redownload corrupted downloads with the correct file size.
  [Mailing list discussion](https://groups.google.com/d/msg/haskell-stack/iVGDG5OHYxs/FjUrR5JsDQAJ)
- Gracefully handle invalid paths in error/warning messages
  [#1561](https://github.com/commercialhaskell/stack/issues/1561)
- Nix: select the correct GHC version corresponding to the snapshot
  even when an abstract resolver is passed via `--resolver` on the
  command-line.
  [#1641](https://github.com/commercialhaskell/stack/issues/1641)
- Fix: Stack does not allow using an external package from ghci
  [#1557](https://github.com/commercialhaskell/stack/issues/1557)
- Disable ambiguous global '--resolver' option for 'stack init'
  [#1531](https://github.com/commercialhaskell/stack/issues/1531)
- Obey `--no-nix` flag
- Fix: GHCJS Execute.hs: Non-exhaustive patterns in lambda
  [#1591](https://github.com/commercialhaskell/stack/issues/1591)
- Send file-watch and sticky logger messages to stderr
  [#1302](https://github.com/commercialhaskell/stack/issues/1302)
  [#1635](https://github.com/commercialhaskell/stack/issues/1635)
- Use globaldb path for querying Cabal version
  [#1647](https://github.com/commercialhaskell/stack/issues/1647)

## 1.0.0

Release notes:

*  We're calling this version 1.0.0 in preparation for Stackage
   LTS 4.  Note, however, that this does not mean the code's API
   will be stable as this is primarily an end-user tool.

Enhancements:

* Added flag `--profile` flag: passed with `stack build`, it will
  enable profiling, and for `--bench` and `--test` it will generate a
  profiling report by passing `+RTS -p` to the executable(s). Great
  for using like `stack build --bench --profile` (remember that
  enabling profile will slow down your benchmarks by >4x). Run `stack
  build --bench` again to disable the profiling and get proper speeds
* Added flag `--trace` flag: just like `--profile`, it enables
  profiling, but instead of generating a report for `--bench` and
  `--test`, prints out a stack trace on exception. Great for using
  like `stack build --test --trace`
* Nix: all options can be overridden on command line
  [#1483](https://github.com/commercialhaskell/stack/issues/1483)
* Nix: build environments (shells) are now pure by default.
* Make verbosity silent by default in script interpreter mode
  [#1472](https://github.com/commercialhaskell/stack/issues/1472)
* Show a message when resetting git commit fails
  [#1453](https://github.com/commercialhaskell/stack/issues/1453)
* Improve Unicode handling in project/package names
  [#1337](https://github.com/commercialhaskell/stack/issues/1337)
* Fix ambiguity between a stack command and a filename to execute (prefer
  `stack` subcommands)
  [#1471](https://github.com/commercialhaskell/stack/issues/1471)
* Support multi line interpreter directive comments
  [#1394](https://github.com/commercialhaskell/stack/issues/1394)
* Handle space separated pids in ghc-pkg dump (for GHC HEAD)
  [#1509](https://github.com/commercialhaskell/stack/issues/1509)
* Add ghci --no-package-hiding option
  [#1517](https://github.com/commercialhaskell/stack/issues/1517)
* `stack new` can download templates from URL
  [#1466](https://github.com/commercialhaskell/stack/issues/1466)

Bug fixes:

* Nix: stack exec options are passed properly to the stack sub process
  [#1538](https://github.com/commercialhaskell/stack/issues/1538)
* Nix: specifying a shell-file works in any current working directory
  [#1547](https://github.com/commercialhaskell/stack/issues/1547)
* Nix: use `--resolver` argument
* Docker: fix missing image message and '--docker-auto-pull'
* No HTML escaping for "stack new" template params
  [#1475](https://github.com/commercialhaskell/stack/issues/1475)
* Set permissions for generated .ghci script
  [#1480](https://github.com/commercialhaskell/stack/issues/1480)
* Restrict commands allowed in interpreter mode
  [#1504](https://github.com/commercialhaskell/stack/issues/1504)
* stack ghci doesn't see preprocessed files for executables
  [#1347](https://github.com/commercialhaskell/stack/issues/1347)
* All test suites run even when only one is requested
  [#1550](https://github.com/commercialhaskell/stack/pull/1550)
* Edge cases in broken templates give odd errors
  [#1535](https://github.com/commercialhaskell/stack/issues/1535)
* Fix test coverage bug on windows

## 0.1.10.1

Bug fixes:

* `stack image container` did not actually build an image
  [#1473](https://github.com/commercialhaskell/stack/issues/1473)

## 0.1.10.0

Release notes:

* The Stack home page is now at [haskellstack.org](http://haskellstack.org),
  which shows the documentation rendered by readthedocs.org. Note: this
  has necessitated some changes to the links in the documentation's markdown
  source code, so please check the links on the website before submitting a PR
  to fix them.
* The locations of the
  [Ubuntu](http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu)
  and
  [Debian](http://docs.haskellstack.org/en/stable/install_and_upgrade/#debian)
  package repositories have changed to have correct URL semantics according to
  Debian's guidelines
  [#1378](https://github.com/commercialhaskell/stack/issues/1378). The old
  locations will continue to work for some months, but we suggest that you
  adjust your `/etc/apt/sources.list.d/fpco.list` to the new location to avoid
  future disruption.
* [openSUSE and SUSE Linux Enterprise](http://docs.haskellstack.org/en/stable/install_and_upgrade/#suse)
  packages are now available, thanks to [@mimi1vx](https://github.com/mimi1vx).
  Note: there will be some lag before these pick up new versions, as they are
  based on Stackage LTS.

Major changes:

* Support for building inside a Nix-shell providing system dependencies
  [#1285](https://github.com/commercialhaskell/stack/pull/1285)
* Add optional GPG signing on `stack upload --sign` or with
  `stack sig sign ...`

Other enhancements:

* Print latest applicable version of packages on conflicts
  [#508](https://github.com/commercialhaskell/stack/issues/508)
* Support for packages located in Mercurial repositories
  [#1397](https://github.com/commercialhaskell/stack/issues/1397)
* Only run benchmarks specified as build targets
  [#1412](https://github.com/commercialhaskell/stack/issues/1412)
* Support git-style executable fall-through (`stack something` executes
  `stack-something` if present)
  [#1433](https://github.com/commercialhaskell/stack/issues/1433)
* GHCi now loads intermediate dependencies
  [#584](https://github.com/commercialhaskell/stack/issues/584)
* `--work-dir` option for overriding `.stack-work`
  [#1178](https://github.com/commercialhaskell/stack/issues/1178)
* Support `detailed-0.9` tests
  [#1429](https://github.com/commercialhaskell/stack/issues/1429)
* Docker: improved POSIX signal proxying to containers
  [#547](https://github.com/commercialhaskell/stack/issues/547)

Bug fixes:

* Show absolute paths in error messages in multi-package builds
  [#1348](https://github.com/commercialhaskell/stack/issues/1348)
* Docker-built binaries and libraries in different path
  [#911](https://github.com/commercialhaskell/stack/issues/911)
  [#1367](https://github.com/commercialhaskell/stack/issues/1367)
* Docker: `--resolver` argument didn't effect selected image tag
* GHCi: Spaces in filepaths caused module loading issues
  [#1401](https://github.com/commercialhaskell/stack/issues/1401)
* GHCi: cpp-options in cabal files weren't used
  [#1419](https://github.com/commercialhaskell/stack/issues/1419)
* Benchmarks couldn't be run independently of each other
  [#1412](https://github.com/commercialhaskell/stack/issues/1412)
* Send output of building setup to stderr
  [#1410](https://github.com/commercialhaskell/stack/issues/1410)

## 0.1.8.0

Major changes:

* GHCJS can now be used with stackage snapshots via the new `compiler` field.
* Windows installers are now available:
  [download them here](http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)
  [#613](https://github.com/commercialhaskell/stack/issues/613)
* Docker integration works with non-FPComplete generated images
  [#531](https://github.com/commercialhaskell/stack/issues/531)

Other enhancements:

* Added an `allow-newer` config option
  [#922](https://github.com/commercialhaskell/stack/issues/922)
  [#770](https://github.com/commercialhaskell/stack/issues/770)
* When a Hackage revision invalidates a build plan in a snapshot, trust the
  snapshot [#770](https://github.com/commercialhaskell/stack/issues/770)
* Added a `stack config set resolver RESOLVER` command. Part of work on
  [#115](https://github.com/commercialhaskell/stack/issues/115)
* `stack setup` can now install GHCJS on windows. See
  [#1145](https://github.com/commercialhaskell/stack/issues/1145) and
  [#749](https://github.com/commercialhaskell/stack/issues/749)
* `stack hpc report` command added, which generates reports for HPC tix files
* `stack ghci` now accepts all the flags accepted by `stack build`. See
  [#1186](https://github.com/commercialhaskell/stack/issues/1186)
* `stack ghci` builds the project before launching GHCi. If the build fails,
  try to launch GHCi anyway. Use `stack ghci --no-build` option to disable
  [#1065](https://github.com/commercialhaskell/stack/issues/1065)
* `stack ghci` now detects and warns about various circumstances where it is
  liable to fail. See
  [#1270](https://github.com/commercialhaskell/stack/issues/1270)
* Added `require-docker-version` configuration option
* Packages will now usually be built along with their tests and benchmarks. See
  [#1166](https://github.com/commercialhaskell/stack/issues/1166)
* Relative `local-bin-path` paths will be relative to the project's root
  directory, not the current working directory.
  [#1340](https://github.com/commercialhaskell/stack/issues/1340)
* `stack clean` now takes an optional `[PACKAGE]` argument for use in
  multi-package projects. See
  [#583](https://github.com/commercialhaskell/stack/issues/583)
* Ignore cabal_macros.h as a dependency
  [#1195](https://github.com/commercialhaskell/stack/issues/1195)
* Pad timestamps and show local time in --verbose output
  [#1226](https://github.com/commercialhaskell/stack/issues/1226)
* GHCi: Import all modules after loading them
  [#995](https://github.com/commercialhaskell/stack/issues/995)
* Add subcommand aliases: `repl` for `ghci`, and `runhaskell` for `runghc`
  [#1241](https://github.com/commercialhaskell/stack/issues/1241)
* Add typo recommendations for unknown package identifiers
  [#158](https://github.com/commercialhaskell/stack/issues/158)
* Add `stack path --local-hpc-root` option
* Overhaul dependencies' haddocks copying
  [#1231](https://github.com/commercialhaskell/stack/issues/1231)
* Support for extra-package-dbs in 'stack ghci'
  [#1229](https://github.com/commercialhaskell/stack/pull/1229)
* `stack new` disallows package names with "words" consisting solely of numbers
  [#1336](https://github.com/commercialhaskell/stack/issues/1336)
* `stack build --fast` turns off optimizations
* Show progress while downloading package index
  [#1223](https://github.com/commercialhaskell/stack/issues/1223).

Bug fixes:

* Fix: Haddocks not copied for dependencies
  [#1105](https://github.com/commercialhaskell/stack/issues/1105)
* Fix: Global options did not work consistently after subcommand
  [#519](https://github.com/commercialhaskell/stack/issues/519)
* Fix: 'stack ghci' doesn't notice that a module got deleted
  [#1180](https://github.com/commercialhaskell/stack/issues/1180)
* Rebuild when cabal file is changed
* Fix: Paths in GHC warnings not canonicalized, nor those for packages in
  subdirectories or outside the project root
  [#1259](https://github.com/commercialhaskell/stack/issues/1259)
* Fix: unlisted files in tests and benchmarks trigger extraneous second build
  [#838](https://github.com/commercialhaskell/stack/issues/838)

## 0.1.6.0

Major changes:

* `stack setup` now supports building and booting GHCJS from source tarball.
* On Windows, build directories no longer display "pretty" information
  (like x86_64-windows/Cabal-1.22.4.0), but rather a hash of that
  content. The reason is to avoid the 260 character path limitation on
  Windows. See
  [#1027](https://github.com/commercialhaskell/stack/pull/1027)
* Rename config files and clarify their purposes [#969](https://github.com/commercialhaskell/stack/issues/969)
    * `~/.stack/stack.yaml` --> `~/.stack/config.yaml`
    * `~/.stack/global` --> `~/.stack/global-project`
    * `/etc/stack/config` --> `/etc/stack/config.yaml`
    * Old locations still supported, with deprecation warnings
* New command "stack eval CODE", which evaluates to "stack exec ghc -- -e CODE".

Other enhancements:

* No longer install `git` on Windows
  [#1046](https://github.com/commercialhaskell/stack/issues/1046). You
  can still get this behavior by running the following yourself:
  `stack exec -- pacman -Sy --noconfirm git`.
* Typing enter during --file-watch triggers a rebuild [#1023](https://github.com/commercialhaskell/stack/pull/1023)
* Use Haddock's `--hyperlinked-source` (crosslinked source), if available [#1070](https://github.com/commercialhaskell/stack/pull/1070)
* Use Stack-installed GHCs for `stack init --solver` [#1072](https://github.com/commercialhaskell/stack/issues/1072)
* New experimental `stack query` command [#1087](https://github.com/commercialhaskell/stack/issues/1087)
* By default, stack no longer rebuilds a package due to GHC options changes. This behavior can be tweaked with the `rebuild-ghc-options` setting. [#1089](https://github.com/commercialhaskell/stack/issues/1089)
* By default, ghc-options are applied to all local packages, not just targets. This behavior can be tweaked with the `apply-ghc-options` setting. [#1089](https://github.com/commercialhaskell/stack/issues/1089)
* Docker: download or override location of stack executable to re-run in container [#974](https://github.com/commercialhaskell/stack/issues/974)
* Docker: when Docker Engine is remote, don't run containerized processes as host's UID/GID [#194](https://github.com/commercialhaskell/stack/issues/194)
* Docker: `set-user` option to enable/disable running containerized processes as host's UID/GID [#194](https://github.com/commercialhaskell/stack/issues/194)
* Custom Setup.hs files are now precompiled instead of interpreted. This should be a major performance win for certain edge cases (biggest example: [building Cabal itself](https://github.com/commercialhaskell/stack/issues/1041)) while being either neutral or a minor slowdown for more common cases.
* `stack test --coverage` now also generates a unified coverage report for multiple test-suites / packages.  In the unified report, test-suites can contribute to the coverage of other packages.

Bug fixes:

* Ignore stack-built executables named `ghc`
  [#1052](https://github.com/commercialhaskell/stack/issues/1052)
* Fix quoting of output failed command line arguments
* Mark executable-only packages as installed when copied from cache [#1043](https://github.com/commercialhaskell/stack/pull/1043)
* Canonicalize temporary directory paths [#1047](https://github.com/commercialhaskell/stack/pull/1047)
* Put code page fix inside the build function itself [#1066](https://github.com/commercialhaskell/stack/issues/1066)
* Add `explicit-setup-deps` option [#1110](https://github.com/commercialhaskell/stack/issues/1110), and change the default to the old behavior of using any package in the global and snapshot database [#1025](https://github.com/commercialhaskell/stack/issues/1025)
* Precompiled cache checks full package IDs on Cabal < 1.22 [#1103](https://github.com/commercialhaskell/stack/issues/1103)
* Pass -package-id to ghci [#867](https://github.com/commercialhaskell/stack/issues/867)
* Ignore global packages when copying precompiled packages [#1146](https://github.com/commercialhaskell/stack/issues/1146)

## 0.1.5.0

Major changes:

* On Windows, we now use a full MSYS2 installation in place of the previous PortableGit. This gives you access to the pacman package manager for more easily installing libraries.
* Support for custom GHC binary distributions [#530](https://github.com/commercialhaskell/stack/issues/530)
    * `ghc-variant` option in stack.yaml to specify the variant (also
      `--ghc-variant` command-line option)
    * `setup-info` in stack.yaml, to specify where to download custom binary
      distributions (also `--ghc-bindist` command-line option)
    * Note: On systems with libgmp4 (aka `libgmp.so.3`), such as CentOS 6, you
      may need to re-run `stack setup` due to the centos6 GHC bindist being
      treated like a variant
* A new `--pvp-bounds` flag to the sdist and upload commands allows automatic adding of PVP upper and/or lower bounds to your dependencies

Other enhancements:

* Adapt to upcoming Cabal installed package identifier format change [#851](https://github.com/commercialhaskell/stack/issues/851)
* `stack setup` takes a `--stack-setup-yaml` argument
* `--file-watch` is more discerning about which files to rebuild for [#912](https://github.com/commercialhaskell/stack/issues/912)
* `stack path` now supports `--global-pkg-db` and `--ghc-package-path`
* `--reconfigure` flag [#914](https://github.com/commercialhaskell/stack/issues/914) [#946](https://github.com/commercialhaskell/stack/issues/946)
* Cached data is written with a checksum of its structure [#889](https://github.com/commercialhaskell/stack/issues/889)
* Fully removed `--optimizations` flag
* Added `--cabal-verbose` flag
* Added `--file-watch-poll` flag for polling instead of using filesystem events (useful for running tests in a Docker container while modifying code in the host environment. When code is injected into the container via a volume, the container won't propagate filesystem events).
* Give a preemptive error message when `-prof` is given as a GHC option [#1015](https://github.com/commercialhaskell/stack/issues/1015)
* Locking is now optional, and will be turned on by setting the `STACK_LOCK` environment variable to `true` [#950](https://github.com/commercialhaskell/stack/issues/950)
* Create default stack.yaml with documentation comments and commented out options [#226](https://github.com/commercialhaskell/stack/issues/226)
* Out of memory warning if Cabal exits with -9 [#947](https://github.com/commercialhaskell/stack/issues/947)

Bug fixes:

* Hacky workaround for optparse-applicative issue with `stack exec --help` [#806](https://github.com/commercialhaskell/stack/issues/806)
* Build executables for local extra deps [#920](https://github.com/commercialhaskell/stack/issues/920)
* copyFile can't handle directories [#942](https://github.com/commercialhaskell/stack/pull/942)
* Support for spaces in Haddock interface files [fpco/minghc#85](https://github.com/fpco/minghc/issues/85)
* Temporarily building against a "shadowing" local package? [#992](https://github.com/commercialhaskell/stack/issues/992)
* Fix Setup.exe name for --upgrade-cabal on Windows [#1002](https://github.com/commercialhaskell/stack/issues/1002)
* Unlisted dependencies no longer trigger extraneous second build [#838](https://github.com/commercialhaskell/stack/issues/838)

## 0.1.4.1

Fix stack's own Haddocks.  No changes to functionality (only comments updated).

## 0.1.4.0

Major changes:

* You now have more control over how GHC versions are matched, e.g. "use exactly this version," "use the specified minor version, but allow patches," or "use the given minor version or any later minor in the given major release." The default has switched from allowing newer later minor versions to a specific minor version allowing patches. For more information, see [#736](https://github.com/commercialhaskell/stack/issues/736) and [#784](https://github.com/commercialhaskell/stack/pull/784).
* Support added for compiling with GHCJS
* stack can now reuse prebuilt binaries between snapshots. That means that, if you build package foo in LTS-3.1, that binary version can be reused in LTS-3.2, assuming it uses the same dependencies and flags. [#878](https://github.com/commercialhaskell/stack/issues/878)

Other enhancements:

* Added the `--docker-env` argument, to set environment variables in Docker container.
* Set locale environment variables to UTF-8 encoding for builds to avoid "commitBuffer: invalid argument" errors from GHC [#793](https://github.com/commercialhaskell/stack/issues/793)
* Enable transliteration for encoding on stdout and stderr [#824](https://github.com/commercialhaskell/stack/issues/824)
* By default, `stack upgrade` automatically installs GHC as necessary [#797](https://github.com/commercialhaskell/stack/issues/797)
* Added the `ghc-options` field to stack.yaml [#796](https://github.com/commercialhaskell/stack/issues/796)
* Added the `extra-path` field to stack.yaml
* Code page changes on Windows only apply to the build command (and its synonyms), and can be controlled via a command line flag (still defaults to on) [#757](https://github.com/commercialhaskell/stack/issues/757)
* Implicitly add packages to extra-deps when a flag for them is set [#807](https://github.com/commercialhaskell/stack/issues/807)
* Use a precompiled Setup.hs for simple build types [#801](https://github.com/commercialhaskell/stack/issues/801)
* Set --enable-tests and --enable-benchmarks optimistically [#805](https://github.com/commercialhaskell/stack/issues/805)
* `--only-configure` option added [#820](https://github.com/commercialhaskell/stack/issues/820)
* Check for duplicate local package names
* Stop nagging people that call `stack test` [#845](https://github.com/commercialhaskell/stack/issues/845)
* `--file-watch` will ignore files that are in your VCS boring/ignore files [#703](https://github.com/commercialhaskell/stack/issues/703)
* Add `--numeric-version` option

Bug fixes:

* `stack init --solver` fails if `GHC_PACKAGE_PATH` is present [#860](https://github.com/commercialhaskell/stack/issues/860)
* `stack solver` and `stack init --solver` check for test suite and benchmark dependencies [#862](https://github.com/commercialhaskell/stack/issues/862)
* More intelligent logic for setting UTF-8 locale environment variables [#856](https://github.com/commercialhaskell/stack/issues/856)
* Create missing directories for `stack sdist`
* Don't ignore .cabal files with extra periods [#895](https://github.com/commercialhaskell/stack/issues/895)
* Deprecate unused `--optimizations` flag
* Truncated output on slow terminals [#413](https://github.com/commercialhaskell/stack/issues/413)

## 0.1.3.1

Bug fixes:

* Ignore disabled executables [#763](https://github.com/commercialhaskell/stack/issues/763)

## 0.1.3.0

Major changes:

* Detect when a module is compiled but not listed in the cabal file ([#32](https://github.com/commercialhaskell/stack/issues/32))
    * A warning is displayed for any modules that should be added to `other-modules` in the .cabal file
    * These modules are taken into account when determining whether a package needs to be built
* Respect TemplateHaskell addDependentFile dependency changes ([#105](https://github.com/commercialhaskell/stack/issues/105))
    * TH dependent files are taken into account when determining whether a package needs to be built.
* Overhauled target parsing, added `--test` and `--bench` options [#651](https://github.com/commercialhaskell/stack/issues/651)
    * For details, see [Build commands documentation](http://docs.haskellstack.org/en/stable/build_command/)

Other enhancements:

* Set the `HASKELL_DIST_DIR` environment variable [#524](https://github.com/commercialhaskell/stack/pull/524)
* Track build status of tests and benchmarks [#525](https://github.com/commercialhaskell/stack/issues/525)
* `--no-run-tests` [#517](https://github.com/commercialhaskell/stack/pull/517)
* Targets outside of root dir don't build [#366](https://github.com/commercialhaskell/stack/issues/366)
* Upper limit on number of flag combinations to test [#543](https://github.com/commercialhaskell/stack/issues/543)
* Fuzzy matching support to give better error messages for close version numbers [#504](https://github.com/commercialhaskell/stack/issues/504)
* `--local-bin-path` global option. Use to change where binaries get placed on a `--copy-bins` [#342](https://github.com/commercialhaskell/stack/issues/342)
* Custom snapshots [#111](https://github.com/commercialhaskell/stack/issues/111)
* --force-dirty flag: Force treating all local packages as having dirty files (useful for cases where stack can't detect a file change)
* GHC error messages: display file paths as absolute instead of relative for better editor integration
* Add the `--copy-bins` option [#569](https://github.com/commercialhaskell/stack/issues/569)
* Give warnings on unexpected config keys [#48](https://github.com/commercialhaskell/stack/issues/48)
* Remove Docker `pass-host` option
* Don't require cabal-install to upload [#313](https://github.com/commercialhaskell/stack/issues/313)
* Generate indexes for all deps and all installed snapshot packages [#143](https://github.com/commercialhaskell/stack/issues/143)
* Provide `--resolver global` option [#645](https://github.com/commercialhaskell/stack/issues/645)
    * Also supports `--resolver nightly`, `--resolver lts`, and `--resolver lts-X`
* Make `stack build --flag` error when flag or package is unknown [#617](https://github.com/commercialhaskell/stack/issues/617)
* Preserve file permissions when unpacking sources [#666](https://github.com/commercialhaskell/stack/pull/666)
* `stack build` etc work outside of a project
* `list-dependencies` command [#638](https://github.com/commercialhaskell/stack/issues/638)
* `--upgrade-cabal` option to `stack setup` [#174](https://github.com/commercialhaskell/stack/issues/174)
* `--exec` option [#651](https://github.com/commercialhaskell/stack/issues/651)
* `--only-dependencies` implemented correctly [#387](https://github.com/commercialhaskell/stack/issues/387)

Bug fixes:

* Extensions from the `other-extensions` field no longer enabled by default [#449](https://github.com/commercialhaskell/stack/issues/449)
* Fix: haddock forces rebuild of empty packages [#452](https://github.com/commercialhaskell/stack/issues/452)
* Don't copy over executables excluded by component selection [#605](https://github.com/commercialhaskell/stack/issues/605)
* Fix: stack fails on Windows with git package in stack.yaml and no git binary on path [#712](https://github.com/commercialhaskell/stack/issues/712)
* Fixed GHCi issue: Specifying explicit package versions (#678)
* Fixed GHCi issue: Specifying -odir and -hidir as .stack-work/odir (#529)
* Fixed GHCi issue: Specifying A instead of A.ext for modules (#498)

## 0.1.2.0

* Add `--prune` flag to `stack dot` [#487](https://github.com/commercialhaskell/stack/issues/487)
* Add `--[no-]external`,`--[no-]include-base` flags to `stack dot` [#437](https://github.com/commercialhaskell/stack/issues/437)
* Add `--ignore-subdirs` flag to init command [#435](https://github.com/commercialhaskell/stack/pull/435)
* Handle attempt to use non-existing resolver [#436](https://github.com/commercialhaskell/stack/pull/436)
* Add `--force` flag to `init` command
* exec style commands accept the `--package` option (see [Reddit discussion](http://www.reddit.com/r/haskell/comments/3bd66h/stack_runghc_turtle_as_haskell_script_solution/))
* `stack upload` without arguments doesn't do anything [#439](https://github.com/commercialhaskell/stack/issues/439)
* Print latest version of packages on conflicts [#450](https://github.com/commercialhaskell/stack/issues/450)
* Flag to avoid rerunning tests that haven't changed [#451](https://github.com/commercialhaskell/stack/issues/451)
* stack can act as a script interpreter (see [Script interpreter] (https://github.com/commercialhaskell/stack/wiki/Script-interpreter) and [Reddit discussion](http://www.reddit.com/r/haskell/comments/3bd66h/stack_runghc_turtle_as_haskell_script_solution/))
* Add the __`--file-watch`__ flag to auto-rebuild on file changes [#113](https://github.com/commercialhaskell/stack/issues/113)
* Rename `stack docker exec` to `stack exec --plain`
* Add the `--skip-msys` flag [#377](https://github.com/commercialhaskell/stack/issues/377)
* `--keep-going`, turned on by default for tests and benchmarks [#478](https://github.com/commercialhaskell/stack/issues/478)
* `concurrent-tests: BOOL` [#492](https://github.com/commercialhaskell/stack/issues/492)
* Use hashes to check file dirtiness [#502](https://github.com/commercialhaskell/stack/issues/502)
* Install correct GHC build on systems with libgmp.so.3 [#465](https://github.com/commercialhaskell/stack/issues/465)
* `stack upgrade` checks version before upgrading [#447](https://github.com/commercialhaskell/stack/issues/447)

## 0.1.1.0

* Remove GHC uncompressed tar file after installation [#376](https://github.com/commercialhaskell/stack/issues/376)
* Put stackage snapshots JSON on S3 [#380](https://github.com/commercialhaskell/stack/issues/380)
* Specifying flags for multiple packages [#335](https://github.com/commercialhaskell/stack/issues/335)
* single test suite failure should show entire log [#388](https://github.com/commercialhaskell/stack/issues/388)
* valid-wanted is a confusing option name [#386](https://github.com/commercialhaskell/stack/issues/386)
* stack init in multi-package project should use local packages for dependency checking [#384](https://github.com/commercialhaskell/stack/issues/384)
* Display information on why a snapshot was rejected [#381](https://github.com/commercialhaskell/stack/issues/381)
* Give a reason for unregistering packages [#389](https://github.com/commercialhaskell/stack/issues/389)
* `stack exec` accepts the `--no-ghc-package-path` parameter
* Don't require build plan to upload [#400](https://github.com/commercialhaskell/stack/issues/400)
* Specifying test components only builds/runs those tests [#398](https://github.com/commercialhaskell/stack/issues/398)
* `STACK_EXE` environment variable
* Add the `stack dot` command
* `stack upgrade` added [#237](https://github.com/commercialhaskell/stack/issues/237)
* `--stack-yaml` command line flag [#378](https://github.com/commercialhaskell/stack/issues/378)
* `--skip-ghc-check` command line flag [#423](https://github.com/commercialhaskell/stack/issues/423)

Bug fixes:

* Haddock links to global packages no longer broken on Windows [#375](https://github.com/commercialhaskell/stack/issues/375)
* Make flags case-insensitive [#397](https://github.com/commercialhaskell/stack/issues/397)
* Mark packages uninstalled before rebuilding [#365](https://github.com/commercialhaskell/stack/issues/365)

## 0.1.0.0

* Fall back to cabal dependency solver when a snapshot can't be found
* Basic implementation of `stack new` [#137](https://github.com/commercialhaskell/stack/issues/137)
* `stack solver` command [#364](https://github.com/commercialhaskell/stack/issues/364)
* `stack path` command [#95](https://github.com/commercialhaskell/stack/issues/95)
* Haddocks [#143](https://github.com/commercialhaskell/stack/issues/143):
    * Build for dependencies
    * Use relative links
    * Generate module contents and index for all packages in project

## 0.0.3

* `--prefetch` [#297](https://github.com/commercialhaskell/stack/issues/297)
* `upload` command ported from stackage-upload [#225](https://github.com/commercialhaskell/stack/issues/225)
* `--only-snapshot` [#310](https://github.com/commercialhaskell/stack/issues/310)
* `--resolver` [#224](https://github.com/commercialhaskell/stack/issues/224)
* `stack init` [#253](https://github.com/commercialhaskell/stack/issues/253)
* `--extra-include-dirs` and `--extra-lib-dirs` [#333](https://github.com/commercialhaskell/stack/issues/333)
* Specify intra-package target [#201](https://github.com/commercialhaskell/stack/issues/201)

## 0.0.2

* Fix some Windows specific bugs [#216](https://github.com/commercialhaskell/stack/issues/216)
* Improve output for package index updates [#227](https://github.com/commercialhaskell/stack/issues/227)
* Automatically update indices as necessary [#227](https://github.com/commercialhaskell/stack/issues/227)
* --verbose flag [#217](https://github.com/commercialhaskell/stack/issues/217)
* Remove packages (HTTPS and Git) [#199](https://github.com/commercialhaskell/stack/issues/199)
* Config values for system-ghc and install-ghc
* Merge `stack deps` functionality into `stack build`
* `install` command [#153](https://github.com/commercialhaskell/stack/issues/153) and [#272](https://github.com/commercialhaskell/stack/issues/272)
* overriding architecture value (useful to force 64-bit GHC on Windows, for example)
* Overhauled test running (allows cycles, avoids unnecessary recompilation, etc)

## 0.0.1

* First public release, beta quality
