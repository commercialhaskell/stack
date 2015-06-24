The following should be tested minimally before a release is considered good
to go. This list will likely expand over time:

* `stack install && stack clean && stack install --pedantic && stack test --flag stack:integration-tests` on Linux, Windows, and OS X, which covers:
    * Self-hosting
    * Unit tests
    * Integration tests
    * stack can install GHC on Linux, Windows, and OS X
* Ensure that `stack --version` gives the correct version number and Git hash, and does not have a dirty tree
* stack can build the wai repo
* Running `stack build` a second time on either stack or wai is a no-op
* Build something that depends on `happy` (suggestion: `hlint`), since `happy` has special logic for moving around the `dist` directory
* Make sure to bump the version number in the .cabal file and the ChangeLog appropriately

For more information, see: https://github.com/commercialhaskell/stack/issues/324

* Review man page and other documentation for any changes that need to be made. 