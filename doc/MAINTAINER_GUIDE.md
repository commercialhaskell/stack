## Pre-release checks

The following should be tested minimally before a release is considered good
to go:

* Integration tests pass on a representative sample of platforms: `stack test
  --flag stack:integration-tests`. The actual release script will perform a more
  thorough test for every platform/variant prior to uploading, so this is just a
  pre-check
* stack can build the wai repo
* Running `stack build` a second time on either stack or wai is a no-op
* Build something that depends on `happy` (suggestion: `hlint`), since `happy`
  has special logic for moving around the `dist` directory
* Make sure to bump the version number in the .cabal file and the ChangeLog
  appropriately (check for any entries that snuck into the previous version's
  changes)
* In release candidate, remove the Changelog's "unreleased changes" section
* Review documentation for any changes that need to be made
    * Search for old Stack version and replace with new version
* Ensure all `doc/*.md` files are listed in `stack.cabal`'s 'extra-source-files`
* Check that any new Linux distribution versions added to
  `etc/scripts/release.hs` and `etc/scripts/vagrant-releases.sh`
* Check that no new entries need to be added to
  [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
  [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),
  and
  [README.md](https://github.com/commercialhaskell/stack/blob/master/README.md)

## Release process

See
[stack-release-script's README](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/README.md#prerequisites)
for requirements to perform the release, and more details about the tool.

* Create a
  [new draft Github release](https://github.com/commercialhaskell/stack/releases/new)
  with tag `vX.Y.Z` (where X.Y.Z is the stack package's version)

* On each machine you'll be releasing from, set environment variables:
  `GITHUB_AUTHORIZATION_TOKEN`, `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`

* On a machine with Vagrant installed:
    * Run `etc/scripts/vagrant-releases.sh`

* On Mac OS X:
    * Run `etc/scripts/osx-release.sh`

* On Windows:
    * Ensure your working tree is in `C:\stack` (or a similarly short path)
    * Run `etc\scripts\windows-releases.bat`

* Push signed Git tag, matching Github release tag name, e.g.: `git tag -u
  9BEFB442 vX.Y.Z && git push origin vX.Y.Z`

* Reset the `release` branch to the released commit, e.g.: `git merge --ff-only
  vX.Y.Z && git push origin release`

* Publish Github release

* Edit
  [stack-setup-2.yaml](https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml),
  and add the new linux64 stack bindist

* Upload package to Hackage: `stack upload . --pvp-bounds=both`

* On a machine with Vagrant installed:
    * Run `etc/scripts/vagrant-distros.sh`

* Update in Arch Linux's
  [haskell-stack.git](ssh+git://aur@aur.archlinux.org/haskell-stack.git):
  `PKGBUILD` and `.SRCINFO`
    * Be sure to reset `pkgrel` in both files, and update the SHA1 sum

* Submit a PR for the
  [haskell-stack Homebrew formula](https://github.com/Homebrew/homebrew/blob/master/Library/Formula/haskell-stack.rb).
  The commit message should just be `haskell-stack <VERSION>`
      * Note: for v0.1.8.0, check if `pcre` should still be a dependency
          * Also, update the homepage

* Build Windows installers.  See https://github.com/borsboom/stack-installer#readme

* [Build new MinGHC distribution](#build_minghc)

* [Upload haddocks to Hackage](#upload_haddocks), if hackage couldn't build on its own

* Announce to haskell-cafe, commercialhaskell, and haskell-stack mailing lists

# Extra steps

## Upload haddocks to Hackage <a name="upload_haddocks"></a>

* Set `STACKVER` environment variable to the Stack version (e.g. `0.1.6.0`)
* Run:

```
STACKDOCDIR=stack-$STACKVER-docs
rm -rf _release/$STACKDOCDIR
mkdir -p _release
cp -r $(stack path --local-doc-root)/stack-$STACKVER _release/$STACKDOCDIR
sed -i '' 's/href="\.\.\/\([^/]*\)\//href="..\/..\/\1\/docs\//g' _release/$STACKDOCDIR/*.html
(cd _release && tar cvz --format=ustar -f $STACKDOCDIR.tar.gz $STACKDOCDIR)
curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u borsboom \
     --data-binary "@_release/$STACKDOCDIR.tar.gz" \
     "https://hackage.haskell.org/package/stack-$STACKVER/docs"
```

## Update MinGHC <a name="build_minghc"></a>

Full details of prerequisites and steps for building MinGHC are in its
[README](https://github.com/fpco/minghc#building-installers). What follows is an
abbreviated set specifically for including the latest stack version.

* Ensure `makensis.exe` and `signtool.exe` are on your PATH.
* If you edit build-post-install.hs, run `stack exec -- cmd /c build-post-install.bat`
* Set `STACKVER` environment variable to latest Stack verion (e.g. `0.1.6.0`)
* Adjust commands below for new GHC versions
* Run:

```
stack build
stack exec -- minghc-generate 7.10.2 --stack=%STACKVER%
signtool sign /v /n "FP Complete, Corporation" /t "http://timestamp.verisign.com/scripts/timestamp.dll" .build\minghc-7.10.2-i386.exe
stack exec -- minghc-generate 7.10.2 --arch64 --stack=%STACKVER%
signtool sign /v /n "FP Complete, Corporation" /t "http://timestamp.verisign.com/scripts/timestamp.dll" .build\minghc-7.10.2-x86_64.exe
stack exec -- minghc-generate 7.8.4 --stack=%STACKVER%
signtool sign /v /n "FP Complete, Corporation" /t "http://timestamp.verisign.com/scripts/timestamp.dll" .build\minghc-7.8.4-i386.exe
stack exec -- minghc-generate 7.8.4 --arch64 --stack=%STACKVER%
signtool sign /v /n "FP Complete, Corporation" /t "http://timestamp.verisign.com/scripts/timestamp.dll" .build\minghc-7.8.4-x86_64.exe
```

* Upload the built binaries to a new Github release
* Edit [README.md](https://github.com/fpco/minghc/blob/master/README.md#using-the-installer) and update download links
