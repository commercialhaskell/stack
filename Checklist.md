The following should be tested minimally before a release is considered good
to go. This list will likely expand over time:

* Run `scripts/release/release.hs check` on Linux, Windows (32-bit and 64-bit), and OS X. See its
  [README](https://github.com/commercialhaskell/stack/blob/master/etc/release-tool/README.md) for build and invocation instructions.
  This performs the following checks automatically:
    * `stack install && stack clean && stack install --pedantic && stack test --flag stack:integration-tests` on Linux, Windows, and OS X, which covers:
        * Self-hosting
        * Unit tests
        * Integration tests
        * stack can install GHC
    * Working tree is clean.
* Ensure that `stack --version` gives the correct version number and Git hash, and does not have a dirty tree
* stack can build the wai repo
* Running `stack build` a second time on either stack or wai is a no-op
* Build something that depends on `happy` (suggestion: `hlint`), since `happy` has special logic for moving around the `dist` directory
* Make sure to bump the version number in the .cabal file and the ChangeLog appropriately
* Review man page and other documentation for any changes that need to be made.

Release checklist after testing:

* Create a draft Github release with tag `vX.Y.Z` (where X.Y.Z is the stack package's version).
* Run `scripts/release/release.hs upload` on Linux (Debian 7 [Vagrantfile](https://github.com/commercialhaskell/stack/tree/master/etc/vagrant/debian-7-amd64)), Windows (32-bit and 64-bit), and OS X.  This performs the following tasks automatically:
    * Binaries for Linux, Windows, and OS X uploaded to draft Github release.
* Run `stack/release/release.hs ubuntu-upload debian-upload` in Linux (Ubuntu or Debian - [Vagrantfile](https://github.com/commercialhaskell/stack/tree/master/etc/vagrant/debian-7-amd64))
* Run `stack/release/release.hs centos-upload fedora-upload` on Linux (CentOS or Fedora - [Vagrantfile](https://github.com/commercialhaskell/stack/tree/master/etc/vagrant/centos-7-x86_64))
* Upload Arch Linux packages (manual process)

After binaries uploaded:

* Publish Github release.
* Sign Git tag.
* Upload package to Hackage.
* Announce to haskell-cafe, commercialhaskell, and haskell-stack mailing lists.

For more information, see: https://github.com/commercialhaskell/stack/issues/324