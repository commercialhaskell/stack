The following should be tested minimally before a release is considered good
to go. This list will likely expand over time:

* stack self-hosts on Linux, Windows, and OS X
* `stack test --flag stack:integration-tests` passes on Linux, Windows, and OS X
* stack can install GHC on Linux, Windows, and OS X
* stack can build the wai repo
* Running `stack build` a second time on either stack or wai is a no-op
* Build something that depends on `happy` (suggestion: `hlint`), since `happy` has special logic for moving around the `dist` directory
* Make sure to bump the version number in the .cabal file and the ChangeLog appropriately