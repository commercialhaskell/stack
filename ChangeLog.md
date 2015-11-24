# Changelog

## Unreleased changes

Major changes:

Other enhancements:

* Print latest applicable version of packages on conflicts
  [#508](https://github.com/commercialhaskell/stack/issues/508)
* Support for packages located in Mercurial repositories
  [#1397](https://github.com/commercialhaskell/stack/issues/1397)

Bug fixes:

## 0.1.8.0

Major changes:

* GHCJS can now be used with stackage snapshots via the new `compiler` field.
* Windows installers are now available:
  [download them here](http://docs.haskellstack.org/en/stable/install_and_upgrade.html#windows)
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
  optimistically launch GHCi anyway. Use `stack ghci --no-build` option to
  disable [#1065](https://github.com/commercialhaskell/stack/issues/1065)
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
* Enable translitation for encoding on stdout and stderr [#824](https://github.com/commercialhaskell/stack/issues/824)
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
    * For details, see [Build commands documentation](http://docs.haskellstack.org/en/stable/build_command.html)

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
