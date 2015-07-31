## 0.1.2.2

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
* Generate indexes for all deps and all installed snapshot packages [#143](https://github.com/fpco/commercialhaskell/issues/143)
* Provide `--resolver global` option [#645](https://github.com/commercialhaskell/stack/issues/645)
    * Also supports `--resolver nightly`, `--resolver lts`, and `--resolver lts-X`
* Make `stack build --flag` error when flag or package is unknown [#617](https://github.com/commercialhaskell/stack/issues/617)
* Preserve file permissions when unpacking sources [#666](https://github.com/commercialhaskell/stack/pull/666)
* `stack build` etc work outside of a project
* `list-dependencies` command [#638](https://github.com/commercialhaskell/stack/issues/638)
* `--upgrade-cabal` option to `stack setup` [#174](https://github.com/commercialhaskell/stack/issues/174)

Bug fixes:

* Extensions from the `other-extensions` field no longer enabled by default [#449](https://github.com/commercialhaskell/stack/issues/449)
* Fix: haddock forces rebuild of empty packages [#452](https://github.com/commercialhaskell/stack/issues/452)

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
