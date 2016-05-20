# Maintainer guide

## Pre-release steps

* Ensure `release` and `stable` branches merged to `master`
* Ensure integration tests pass on a representative Windows, Mac OS X, and Linux (Linux
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
      ([this comparison](https://github.com/commercialhaskell/stack/compare/stable...master)
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
        * [Ubuntu](https://wiki.ubuntu.com/Releases)
        * [Debian](https://www.debian.org/releases/) (keep at least latest two)
        * [CentOS](https://wiki.centos.org/Download)
        * [Fedora](https://fedoraproject.org/wiki/Releases)
    * Check for new FreeBSD release
    * Check that no new entries need to be added to
      [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
      [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),
      and
      `README.md`
    * Remove unsupported/obsolete distribution versions from
      [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),
      and perhaps from the release process.

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

* Publish Github release

* Upload package to Hackage: `stack upload . --pvp-bounds=both`

* On a machine with Vagrant installed:
    * Run `etc/scripts/vagrant-distros.sh`

* Edit
  [stack-setup-2.yaml](https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml),
  and add the new linux64 stack bindist

* Submit a PR for the
  [haskell-stack Homebrew formula](https://github.com/Homebrew/homebrew-core/blob/master/Formula/haskell-stack.rb)
      * Be sure to update the SHA sum
      * The commit message should just be `haskell-stack <VERSION>`

* [Flag the Arch Linux package as out-of-date](https://www.archlinux.org/packages/community/x86_64/stack/flag/)

* Push signed Git tag, matching Github release tag name, e.g.: `git tag -d vX.Y.Z && git tag -u
  0x575159689BEFB442 vX.Y.Z && git push origin vX.Y.Z`

* Reset the `release` branch to the released commit, e.g.: `git checkout release
  && git merge --ff-only vX.Y.Z && git push origin release`

* Update the `stable` branch similarly

* Delete the RC branch (locally and on origin)

* Activate version for new release tag on
  [readthedocs.org](https://readthedocs.org/projects/stack/versions/), and
  ensure that stable documentation has updated

* Upload haddocks to Hackage: `etc/scripts/upload-haddocks.sh`

* Merge any changes made in the RC/release/stable branches to master.

* Announce to haskell-cafe@haskell.org haskell-stack@googlegroups.com
  commercialhaskell@googlegroups.com mailing lists

* Keep an eye on the
  [Hackage matrix builder](http://matrix.hackage.haskell.org/package/stack)

## Setting up a Windows VM for releases

These instructions are a bit rough, but has the steps to get the Windows machine
set up.

 1. Download VM image:
    https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/mac/

 2. Launch the VM using Virtualbox and the image downloaded

 3. Adjust settings:
    * Number of CPUs: match the host
    * Memory: at least 3 GB
    * Video RAM: the minimum recommended by Virtualbox
    * Enable 3D and 2D accelerated mode
    * Enabled shared clipboard (both directions)

 4. Install the VMware guest additions, and reboot

 5. Configure a shared folder for your home directory on the host, and mount it on Z:

 6. Install Windows SDK (for signtool):
    http://microsoft.com/en-us/download/confirmation.aspx?id=8279

 7. Install msysgit: https://msysgit.github.io/

 8. Install nsis-2.46.5-Unicode-setup.exe from http://www.scratchpaper.com/

 9: Install Stack using the Windows 64-bit installer

10. Visit https://hackage.haskell.org/ in Edge to ensure system has correct CA
    certificates

11. Get the object code certificate from
    [password-store](https://github.com/fpco/password-store), in
    `certificates/code_signing/fpcomplete_corporation_startssl_2015-09-22.pfx`.
    Double click it in explorer and import it

12. Run in command prompt:

        md C:\p
        md C:\p\tmp
        cd \p
        md c:\tmp

13. Create `C:\p\env.bat`:

        SET STACK_ROOT=C:\p\.sr
        SET TEMP=C:\p\tmp
        SET TMP=C:\p\tmp
        SET PATH=C:\Users\IEUser\AppData\Roaming\local\bin;"c:\Program Files\Git\usr\bin";"C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin";%PATH%

14. Run `C:\p\env.bat` (do this every time you open a new command prompt)

15. Import the `dev@fpcomplete.com` (0x575159689BEFB442) GPG secret key

16. Run in command prompt (adjust the `user.email` and `user.name` settings):

        stack setup
        stack install cabal-install
        md %HOMEPATH%\.ssh
        copy z:\.ssh\id_rsa %HOMEPATH%\.ssh
        git config --global user.email manny@fpcomplete.com
        git config --global user.name "Emanuel Borsboom"
        git config --global push.default simple
        git config --global core.autocrlf true
        git clone git@github.com:commercialhaskell/stack.git
        git clone git@github.com:borsboom/stack-installer.git
