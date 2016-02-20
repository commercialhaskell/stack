# Maintainer guide

## Next release:

* Integrate FreeBSD binaries and packages
  [#1253](https://github.com/commercialhaskell/stack/issues/1253#issuecomment-185993240)

## Pre-release checks

The following should be tested minimally before a release is considered good
to go:

* Ensure `release` and `stable` branches merged to `master`
* Integration tests pass on a representative Windows, Mac OS X, and Linux (Linux
  is handled by Jenkins automatically): `stack install --pedantic && stack test
  --pedantic --flag stack:integration-tests` . The actual release script will
  perform a more thorough test for every platform/variant prior to uploading, so
  this is just a pre-check
* Ensure `stack haddock` works (Travis CI now does this)
* Stack builds with `stack-7.8.yaml` (Travis CI now does this)
* stack can build the wai repo
* Running `stack build` a second time on either stack or wai is a no-op
* Build something that depends on `happy` (suggestion: `hlint`), since `happy`
  has special logic for moving around the `dist` directory
* In master branch:
    * stack.cabal: bump the version number to release (even third
      component)
    * ChangeLog: rename the "unreleased changes" section to the new version
* Cut a release candidate branch `rc/vX.Y.Z` from master
* In master branch:
    * stack.cabal: bump version number to unstable (odd third component)
    * Changelog: add new "unreleased changes" section
    * stack.yaml: bump to use latest LTS version, and check whether extra-deps
      still needed
* In RC branch:
    * Update the ChangeLog
      ([this comparison](https://github.com/commercialhaskell/stack/compare/release...master)
      is handy):
        * Check for any important changes that missed getting an entry in Changelog
        * Check for any entries that snuck into the previous version's changes
          due to merges
    * Review documentation for any changes that need to be made
        * Search for old Stack version, unstable stack version, and the next
          "obvious" version in sequence (if doing a non-obvious jump) and replace
          with new version
        * Look for any links to "latest" documentation, replace with version tag
        * Ensure all documentation pages listed in `mkdocs.yaml`
    * Check that any new Linux distribution versions added to
      `etc/scripts/release.hs` and `etc/scripts/vagrant-releases.sh`
    * Check that no new entries need to be added to
      [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
      [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),
      and
      `README.md`

## Release process

See
[stack-release-script's README](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/README.md#prerequisites)
for requirements to perform the release, and more details about the tool.

* Create a
  [new draft Github release](https://github.com/commercialhaskell/stack/releases/new)
  with tag and name `vX.Y.Z` (where X.Y.Z is the stack package's version), targetting the
  RC branch

* On each machine you'll be releasing from, set environment variables:
  `GITHUB_AUTHORIZATION_TOKEN`, `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
  `AWS_DEFAULT_REGION`.

    Note: since one of the tools (rpm-s3 on CentOS) doesn't support AWS temporary
    credentials, you can't use MFA with the AWS credentials (`AWS_SECURITY_TOKEN`
    is ignored).

* On a machine with Vagrant installed:
    * Run `etc/scripts/vagrant-releases.sh`

* On Mac OS X:
    * Run `etc/scripts/osx-release.sh`

* On Windows:
    * Ensure your working tree is in `C:\stack` (or a similarly short path)
    * Run `etc\scripts\windows-releases.bat`
    * Release Windows installers. See
      [stack-installer README](https://github.com/borsboom/stack-installer#readme)

* Push signed Git tag, matching Github release tag name, e.g.: `git tag -u
  9BEFB442 vX.Y.Z && git push origin vX.Y.Z`

* Reset the `release` branch to the released commit, e.g.: `git checkout release
  && git merge --ff-only vX.Y.Z && git push origin release`

* Update the `stable` branch similarly

* Delete the RC branch (locally and on origin)

* Publish Github release

* Edit
  [stack-setup-2.yaml](https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml),
  and add the new linux64 stack bindist

* Activate version for new release tag on
  [readthedocs.org](https://readthedocs.org/projects/stack/versions/), and
  ensure that stable documentation has updated

* Upload package to Hackage: `stack upload . --pvp-bounds=both`

* On a machine with Vagrant installed:
    * Run `etc/scripts/vagrant-distros.sh`

* Submit a PR for the
  [haskell-stack Homebrew formula](https://github.com/Homebrew/homebrew/blob/master/Library/Formula/haskell-stack.rb)
      * Be sure to update the SHA sum
      * The commit message should just be `haskell-stack <VERSION>`

* [Flag the Arch Linux package as out-of-date](https://www.archlinux.org/packages/community/x86_64/stack/flag/)

* Upload haddocks to Hackage: `etc/scripts/upload-haddocks.sh`

* Merge any changes made in the RC/release/stable branches to master.

* Announce to haskell-cafe@haskell.org haskell-stack@googlegroups.com
  commercialhaskell@googlegroups.com mailing lists

* Keep an eye on the
  [Hackage matrix builder](http://matrix.hackage.haskell.org/package/stack)
