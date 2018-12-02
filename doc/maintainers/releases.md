<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Releases

## Upcoming release tasks:

* Add back support for building static Linux binaries (by popular demand), and update `get-stack.sh` to use it (see [#4088](https://github.com/commercialhaskell/stack/issues/4088)).
* Check whether `persistent` still needs `monad-logger`; remove dependency if not
* Check if workaround for https://github.com/commercialhaskell/stack/issues/3922 still needed in stack.yaml
* Eventually remove the Ubuntu, Debian, CentOS, Arch packages from our S3 bucket.  This was announced with the 1.9.x release, so can do this around time of 1.11.x.  Directories, and last Stack version uploaded:
	- `s3://download.fpcomplete.com/archlinux` (1.0.0)
	- `s3://download.fpcomplete.com/centos` (1.5.1)
	- `s3://download.fpcomplete.com/debian` (1.4.0)
	- `s3://download.fpcomplete.com/fedora` (1.3.0)
	- `s3://download.fpcomplete.com/ubuntu` (1.5.1)
* Eventually remove `-nopie` variants from [stack-setup-2.yaml](https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml).  stack-1.6 was the last release to use them, so wait a few major releases after that, and be sure to announce.
    * Also remove the `-nopie` variants from `etc/scripts/mirrog-ghc-bindists-to-github.sh`.
* Remove workaround to [#4125](https://github.com/commercialhaskell/stack/issues/4125) from `stack.yaml` for next major version after 1.8.
* Explicitly tag release candidates and prereleases in `stack --version` output.
* Look through https://fpcomplete.slack.com/files/U9U8HDGUC/FCM7UN5NJ/notes_on_doc_maintainers_releases_md.txt for hints on how to make this document more clear.
* If `store` is no longer a dependency, likely can remove from stackage build constraints' `expected-test-failures`

## Iterating on release process

**IMPORTANT: all bindists for a given release should be built from a consistent git commit** (this can be relaxed a bit for release candidates, but still should be maintained as much as possible).

Since the release process and scripts sometimes need to be iterated on during the process of building all the platforms' binaries, the scripts are all designed so that you can run them *from a different directory*.  This means you can have one source tree for the version of `stack` being released, and a separate one where you work on the release scripts and process.

To use this way, set the current directory should be the version of `stack` to be released.  You can then call the scripts in another directory to use their version.  For example, `stack ../stack-release-scripts/etc/scripts/release.hs …` or `../stack-release-scripts/etc/scripts/vagrant-releases.hs`.


## Version scheme

* Versions with an _even_ second component are development versions (the `master` branch)
* Versions with an _odd_ second component are stable versions (the `stable` branch, or in a `vX.Y` release candidate branch for not-yet-released versions)
* Versions with an _even_ third component (e.g. 1.6.2 and 1.7.0) are unreleased versions
* Versions with an _odd_ third component (e.g. 1.6.1 or 1.7.3) and released versions
* Pre-release unstable binaries will be released with the date as the fourth component (e.g. 1.6.0.20171129)
* Release candidate binaries will be released with an even third component and and odd number as the fourth component (e.g. 1.7.0.1)
* All branches _except_ `release` (which matches exactly the most recent release) must have an even third component (development)
* Branches other than `stable`, `release`, and a `vX.Y` release candidate will always have a `0` third component (e.g. 1.7.0).

Examples:

* `1.7.0.0`: v1.7.x series pre-release branch (`v1.7` branch)
* `1.7.0.1`: release candidate for first release of v1.7.x series (`v1.7` branch)
* `1.7.0.2`: continuing development on pre-release branch
* `1.7.0.3`: second release candidate for first release of v1.7.x series (`v1.7` branch)
* `1.7.1`: first release of the 1.7.x series (`release` branch)
* `1.7.2.0`: development for second release of 1.7.x series (`stable` branch)
* `1.7.2.1`: release candidate for second release of 1.7.x series (`stable` branch)
* `1.7.3`: second release of 1.7.x series (`release` branch)
* `1.8.0`: unstable development code (`master` branch)
* `1.8.0.20181004`: pre-release snapshot of unstable version (`master` branch)

## Pre-release checks

* Check that the snapshot in `stack.yaml`'s' GHC version supports building on all required platforms (e.g. GHC 8.4 doesn't support FreeBSD, so it's not a candidate).  If not, will have to switch to a different snapshot (or decide that we won't support the platform for this iteration).
* Check for any P0 and P1 issues that should be dealt with before release
* Check for un-merged pull requests that should be merged before release
* Ensure `release` and `stable` branches merged to `master`
* Ensure no bounds specified in package.yaml unless truly necessary (e.g. in most cases rely on stack.yaml's resolver to manage dependency versions), and ensure `base` minbound is correct for the supported GHC version(s) (e.g. use `stack exec ghc-pkg list base` to find out minimum base version)
* Check compatibility with latest LTS Stackage snapshots
    * `stack*.yaml` (where `*` is not `nightly`), __including the ones in
      subdirectories__: bump to use latest LTS minor
      version (be sure any extra-deps that exist only for custom flags have
      versions matching the snapshot)
    * Check for any redundant extra-deps
    * Run `stack --stack-yaml=stack*.yaml test --pedantic` (replace `*` with
      the actual file)
* Check compatibility with latest nightly stackage snapshot:
    * Update `stack-nightly.yaml` with latest nightly and remove unnecessary extra-deps (be
      sure any extra-deps that exist only for custom flags have versions
      matching the snapshot)
    * Run `stack --stack-yaml=stack-nightly.yaml test --pedantic`
* Update `.travis.yml` for any GHC version changes made in above steps.
* Ensure integration tests pass on a Windows, macOS, and Linux (Linux
  integration tests are run
  by
  [Gitlab](http://gitlab.fpcomplete.com/fpco-mirrors/stack/pipelines)):
  `stack install --pedantic && stack test --pedantic --flag
  stack:integration-tests`. The actual release script will perform a more
  thorough test for every platform/variant prior to uploading, so this is just a
  pre-check

## Release preparation

* In master branch:
    * `package.yaml`: bump to next release candidate version (bump second component to next odd number, ensure third component is `0`, and add patchlevel `1`; e.g. from `1.8.0` to `1.9.0.1`)
    * `ChangeLog.md`
        * Rename the "Unreleased changes" section to the same version as package.yaml, and mark it clearly as a release candidate (e.g. `v1.9.0.1 (release candidate)`).  Remove any empty sections.
        * Check for any entries that snuck into the previous version's changes
          due to merges (`git diff origin/stable HEAD ChangeLog.md`)

* Cut a release candidate branch `vX.Y` from master

* In master branch:
    * package.yaml: bump version to next unstable version (bump to next even second component with `.0` third component (e.g. from 1.9.0 to 1.10.0)
    * Changelog: add new "Unreleased changes" section:
      ```
      ## Unreleased changes

      Release notes:

      Major changes:

      Behavior changes:

      Other enhancements:

      Bug fixes:
      ```

* In RC branch:
    * Review documentation for any changes that need to be made
        * Ensure all documentation pages listed in `mkdocs.yaml`
          (`git diff --stat origin/stable..HEAD doc/`)
        * Any new documentation pages should have the "may not be correct for
          the released version of Stack" warning at the top.
        * Search for old Stack version, unstable stack version, and the next
          "obvious" possible versions in sequence, and
          `UNRELEASED` and replace with next release version (`X.Y.1`, where Y is odd).
          Note: do **not** update the Dockerfiles in `etc/dockerfiles/stack-build` yet; that will come later)
            * Do __NOT__ update templates in `.github` to point at the new release version yet!
        * Look for any links to "latest" documentation, replace with version tag
    * Update `STACK_VERSION` in `etc/scripts/get-stack.sh` to the new release version (`X.Y.1`)
    * Check if GHC version we're using has bindists using upgraded versions of operating systems (e.g. FreeBSD, Debian) and upgrade relevant Vagrantfiles in `etc/vagrant` and release shell script (`etc/scripts/*-release.sh`) to match.
    * Check that for any platform entries that need to be added to (or removed from)
      [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
      [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md), [get-stack.sh](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/get-stack.sh), and [doc/README.md](https://github.com/commercialhaskell/stack/blob/master/doc/README.md).

* Follow steps in *Release process* below tagged with `[RC]` to make a release candidate

* For subsequent release candidates:
    * Re-do the pre-release checks (above section)
    * `package.yaml`: bump to next odd patchlevel version (e.g. `X.Y.0.3`)
    * `ChangeLog.md`: Rename the "Unreleased changes" section to the new version, clearly marked as a release candidate (e.g. `vX.Y.0.3 (release candidate)`).  Remove any empty sections.
    * Follow steps in *Release process* below tagged with `[RC]` to make a release candidate

* For final release:
    * `package.yaml`: bump version to odd last component and no patchlevel (e.g. from `X.Y.0.2` to `X.Y.1`).
    * `ChangeLog.md`: consolidate all the RC changes into a single section for the release version
    * Follow all steps in the *Release process* section below.


## Release process

See
[stack-release-script's README](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/README.md#prerequisites)
for requirements to perform the release, and more details about the tool.

* Create a
  [new draft Github release](https://github.com/commercialhaskell/stack/releases/new)
  with tag and name `vX.Y.Z` (where X.Y.Z matches the version in `package.yaml` from the previous step), targeting the RC branch.  In the case of a release candidate, add `(RELEASE CANDIDATE)` to the name field.  check the *This is a pre-release* checkbox.  `[RC]`

* On each machine you'll be releasing from, set environment variables `GITHUB_AUTHORIZATION_TOKEN` and `STACK_RELEASE_GPG_KEY` (see [stack-release-script's README](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/README.md#prerequisites)). `[RC]`

* [TODO (for below steps): All the `etc/scripts/*-releases.sh` should be integrated into `etc/scripts/release.hs`]

* On a machine with Vagrant installed: `[RC]`
    * Run `etc/scripts/vagrant-releases.sh`

* On macOS: `[RC]`
    * Run `etc/scripts/osx-release.sh`

* On Windows: `[RC]`
    * Use a short path for your working tree (e.g. `C:\p\stack-release`
    * Ensure that STACK_ROOT, TEMP, and TMP are set to short paths
    * Run `etc\scripts\windows-releases.bat` (for release candidates, only 64-bit is necessary so feel free to comment out 32-bit)
    * Release Windows installers. See
      [stack-installer README](https://github.com/borsboom/stack-installer#readme).
      For release candidates, the windows installers can be skipped.
      [TODO: this should be integrated into `etc/scripts/release.hs`]

* On Linux ARMv7: `[RC]`
    * Run `etc/scripts/linux-armv7-release.sh`

* On Linux ARM64 (aarch64): `[RC]`
    * Run `etc/scripts/linux-aarch64-release.sh`

* Build a Linux static bindist `[RC]`
    * Follow directions in the **Build Linux static binary distribution with Nix** section below.

* Build sdist using `stack sdist .`, and upload it to the
  Github release with a name like `stack-X.Y.Z-sdist-0.tar.gz`.  Also upload GPG signature and checksums. `[RC]`

  TODO: did this last time by copying to `_release` and then using `release.hs` to upload sigs and checksum -- should add all this logic to `release.hs` itself.  e.g.:

  ```
  mv /home/vagrant/stack-release/.stack-work/dist/x86_64-linux/Cabal-2.0.1.0/stack-1.9.0.1.tar.gz _release/stack-1.9.0.1-sdist-0.tar.gz
  stack ../stack/etc/scripts/release.hs _release/stack-1.9.0.1-sdist-0.tar.gz.upload _release/stack-1.9.0.1-sdist-0.tar.gz.asc.upload _release/stack-1.9.0.1-sdist-0.tar.gz.sha256.upload
  ```

* Use `etc/scripts/sdist-with-bounds.sh` to generate a Cabal spec and sdist with dependency bounds.  `[RC]` [TODO: should add this logic to `release.hs` itself]

* Upload `_release/stack-X.Y.Z-sdist-1.tar.gz` to the Github release similarly to the `sdist-0` above. `[RC]`

* For any GPG key used to sign an uploaded bindist, ensure that `dev@fpcomplete.com` signs their key and uploads to keyserver:

  ```
  gpg --sign-key -u 0x575159689BEFB442 <OTHER-KEY-ID>
  gpg --send-keys <OTHER-KEY-ID>
  ```

* Publish Github release. Include the changelog and in the description and use e.g. `git shortlog -s origin/release..HEAD|sed $'s/^[0-9 \t]*/* /'|LC_ALL=C sort -f` to get the list of contributors (contributors not necessary for release candidates). See previous releases for example formatting and extra info (such as link to website for install instructions).  `[RC]`

* Push signed Git tag, matching Github release tag name, e.g.: `git tag -d vX.Y.Z; git tag -s -m vX.Y.Z vX.Y.Z && git push -f origin vX.Y.Z`.  `[RC]`

* Upload package to Hackage: `stack upload .` (note that this uploads revision 0 without bounds, which is best for stackage since we have told it to ignore revisions for stack)

* [Edit Cabal file metadata on Hackage](http://hackage.haskell.org/package/stack/maintain) and paste in the contents of `_release/stack-X.Y.Z_bounds.cabal` to add bounds (this helps non-stackage users build stack).  You will have to add the `x-revision: 1` field.

* Reset the `release` branch to the released commit, e.g.: `git checkout release && git merge --ff-only vX.Y.Z && git push origin release`

* Update the `stable` branch similarly

* In the `stable` or, in the case of a release candidate, `vX.Y` branch: `[RC]`
    * `package.yaml`: bump the version number even third component (e.g. from 1.6.1 to 1.6.2) or, in the case of a release candidate even _fourth_ component (e.g. from 1.7.0.1 to 1.7.0.2).
    * `ChangeLog.md`: Add an "Unreleased changes" section:

        ```
        ## Unreleased changes

        Release notes:

        Major changes:

        Behavior changes:

        Other enhancements:

        Bug fixes:
        ```

* Activate version for new release tag (or, in the case of release candidates, the `vX.Y` branch), on
  [readthedocs.org](https://readthedocs.org/dashboard/stack/versions/), and
  ensure that stable documentation has updated.  `[RC]`

* Deactivate version for release candidate on [readthedocs.org](https://readthedocs.org/dashboard/stack/versions/).

* Update [get.haskellstack.org /stable rewrite rules](https://gitlab.fpcomplete.com/fpco/devops/blob/develop/dockerfiles/nginx/prod-v2/etc/nginx/conf.d/haskellstack.conf) (be sure to change both places) to new released version, and update production cluster.

* Delete the RC branch (locally and on origin).  E.g. `git branch -d vX.Y; git push origin :vX.Y`.

* Update fpco/stack-build Docker images with new version
    * Add `etc/dockerfiles/stack-build/lts-X.Y/Dockerfile` (where `X.Y` is the latest stackage LTS version), containing (note where X.Z is the previous LTS version, and X.Y.Z is the newly released stack version)

      ```
      FROM fpco/stack-build:lts-X.Z
      ARG STACK_VERSION=X.Y.Z
      RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_        VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
      ```

    * Run the appropriate job for the LTS major version in [Gitlab pipelines](https://gitlab.fpcomplete.com/fpco-mirrors/stack/pipelines).

    * Check that the newly build Docker image has the new Stack version

* Merge any changes made in the RC/release/stable branches to master (be careful about version and changelog).  `[RC]`

* `master` branch: update templates in `.github` to point at the new release version (`X.Y.1`).

* Announce to haskell-cafe@haskell.org, haskell-stack@googlegroups.com,
  commercialhaskell@googlegroups.com mailing lists, subject `ANN: stack-X.Y.Z` (or `ANN: stack-X.Y release candidate`), containing the markdown for the release description from Github. `[RC]`

* Add back to Stackage nightly if fallen out (be sure to have a `< 9.9.9` constraint to avoid the accidentally uploaded stack-9.9.9 from being used).


## Build Linux static binary distribution with Nix

TODO: script this process and/or integrate with `etc/scripts/release.hs`

TODO: run integration tests against static binary


These instructions are tested on Ubuntu 16.04, but theoretically should work on any Linux distribution.

- Install nix (tested with v2.0.4 and v2.1.2, but should work with any)

  ```
  curl https://nixos.org/nix/install | sh
  ```

- Install and authenticate cachix (first two steps at https://cachix.org/ after signing up)


- Add nh2's cache:

  ```
  cachix use static-haskell-nix
  ```

  NOTE: to clear cache index, use `rm $HOME/.cache/nix/binary-cache-v5.sqlite*` (useful if someone else uploads new stuff to the cache and you want to use it right away).  The recent `narinfo-cache-positive`/`negative-ttl` options might also help.

- Check out stack commit to be released to `~/stack-release` (or elsewhere, in which case adjust following instructions)

- `rm -f ~/stack-release/stack.cabal`, to ensure it's regenerated

- clone https://github.com/nh2/static-haskell-nix (commit 6bd86c02ed11cc34f209430e23761cd14461c067)

- in `static-stack` directory, run (from https://github.com/nh2/static-haskell-nix/blob/upstream-nixpkgs-musl-1.1.19/static-stack/README.md#Building):

  ```
  $(nix-build --no-out-link -A stack2nix-script) ~/stack-release
  nix-build --no-out-link default.nix -A static_stack --arg release true
  ```

- Copy the binary built above (in `/nix/store/XXX-stack-X.Y.Z/bin/stack`) to `~/.stack-release/_release/bin/stack-X.Y.Z-linux-x86_64-static/stack` (replace `X.Y.Z` with the version, and the `/nix/store/*` path with that output at the end of the previous command)

- Package, sign, and upload to Github using stack's release script in the stack directory:

  ```
  cd ~/stack-release
  stack etc/scripts/release.hs --no-test-haddocks --binary-variant=static --build-args=--dry-run upload
  ```

  (adding `--build-args=--dry-run` ensures the binary you copied will be used rather than building a new one)

- Download the bindist from github and double check that the `stack` in it is actually static (use `ldd /path/to/stack`) and that `--version` reports correctly (and not dirty).


## Setting up a Windows VM for releases

These instructions are a bit rough, but has the steps to get the Windows machine
set up.

## Using Virtualbox

 1. Download Virtualbox VM image:
    https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/mac/

 2. Launch the VM using Virtualbox and the image downloaded

 3. Adjust settings:
    * Number of CPUs: at least half the host's
    * Memory: at least 3 GB
    * Video RAM: the minimum recommended by Virtualbox
    * Enable 3D and 2D accelerated mode (this makes programs with lots of console output much faster)
    * Enabled shared clipboard (in VM window, Devices->Shared Clipboard->Both Directions)

Now continue to the **General Windows setup** subsection below.

## Using ESXi

1. Download the **MSEdge on Win10** VM for **VMWare (Windows, Mac)**.
2. Unzip the file downloaded file
3. Upload the VMDK file to the ESXi datastore
4. SSH into ESXi CLI and run:
    - `vmkfstools -i /vmfs/volumes/datastore1/win10-msedge/MSEdge-Win10-VMWare-disk1-ORIG.vmdk /vmfs/volumes/datastore1/win10-msedge/MSEdge-Win10-VMWare-disk1.vmdk -d thin`.  This converts the disk to a format that is compatible with ESXi.  You may have to run `esxcli system module load -m multiextent` first (see https://www.virtuallyghetto.com/2012/09/2gbsparse-disk-format-no-longer-working.html).
    - `vmkfstools -X 80G /vmfs/volumes/datastore1/win10-msedge/MSEdge-Win10-VMWare-disk1.vmdk`.  This makes the disk twice as large, which helps avoid running out of disk space.
5. In the ESXi web UI:
	- Create a new VM
		- Give is 8192 MB of memory
		- Give it 4 virtual CPUs
		- Remove the default hard disk
		- Add an **Existing hard disk**
			- Select `/datastore1/win10-msedge/MSEdge-Win10-VMWare-disk1.vmdk`
	- Power on the VM
	- In Windows settings:
		- Search for "disk management"
			- Extend the partition to take the whole disk.
		- In all likelyhood, you will want to search for "remote desktop" and enable remote desktop.  Then you can connect to the VM using Microsoft Remote Desktop instead of using it from within the ESXi web UI.

Now continue to the **General Windows setup** subsection below.

## General Windows setup

 5. In **Settings**->**Update & Security**->**Windows Update**->**Advanced options**:
     * Change **Choose how updates are installed** to **Notify to schedule restart**
     * Check **Defer upgrades** (this avoids rebooting in the middle of the stack build)

6. In **Settings**->**System**->**Power & sleep**
	- Disable turning off the screen or going to sleep when plugged in

 8. Install msysgit: https://msysgit.github.io/

 9. Install nsis-2.46.5-Unicode-setup.exe from http://www.scratchpaper.com/

10. Install Stack using the Windows 64-bit installer

    a. Restart any command prompts to ensure they get new `%STACK_ROOT%` value.

11. Visit https://hackage.haskell.org/ in Edge to ensure system has correct CA
    certificates

13. Run in command prompt:

        md C:\p
        md C:\tmp
        cd /d C:\p

14. Create `C:\p\env.bat`:

        SET TEMP=C:\tmp
        SET TMP=C:\tmp
        SET PATH=C:\Users\IEUser\AppData\Roaming\local\bin;"c:\Program Files\Git\usr\bin";"C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin";%PATH%

15. Run `C:\p\env.bat` (do this every time you open a new command prompt)

16. `stack exec -- gpg --import`, and paste in the your GPG secret key (must be done using `stack exec` because that uses the right keyring for the embedded msys GPG; you can get the key from another machine with `gpg --export-secret-keys --armor <KEY ID>`)

17. Run in command prompt (adjust the `user.email` and `user.name` settings):

        git config --global user.email manny@fpcomplete.com
        git config --global user.name "Emanuel Borsboom"
        git config --global push.default simple
        git config --global core.autocrlf true
        git clone https://github.com/commercialhaskell/stack.git stack-release
        git clone https://github.com/borsboom/stack-installer.git
        cd stack-release
        stack install cabal-install


## Setting up an ARM VM for releases

1. Use Scaleway to start ARMv7 and ARM64 VMs.

2. Select Ubuntu Xenial as the operating system

3. Install the correct version of LLVM: `sudo apt-get install -y llvm-3.9` (appropriate for GHC 8.2, might need different version for other GHCs)

4. Symlink opt-3.X to `opt`: `sudo ln -s opt-3.9 /usr/bin/opt` (adjust the version if you installed a different one above)

5. Switch to gold linker:

    ```
    update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
    update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
    update-alternatives --config ld
    ```

6. Add swap space:

    ```
    dd if=/dev/zero of=/swapfile1 bs=1024 count=4194304
    mkswap /swapfile1
    swapon /swapfile1
    echo '/swapfile1 none swap sw 0 0' >>/etc/fstab
    ```

7. Install additional tools:

    ```
    apt-get update && apt-get install -y unzip gpg
    ```

8. Import your GPG key (`gpg --import` and paste the private key)

9. Git settings (adjust for your preferences/email/name)

    git config --global push.default simple
    git config --global user.email "manny@fpcomplete.com"
    git config --global user.name "Emanuel Borsboom"

10. Install build tools and dependencies packages

    sudo apt-get install -y g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg

11. Install clang+llvm

    NOTE: the Debian jessie `llvm` package does not work (executables built with it
    just exit with "schedule: re-entered unsafely.").

    The version of LLVM needed depends on the version of GHC you need.

    * GHC 8.0.2 (the standard for building Stack)

      ```
      wget http://llvm.org/releases/3.7.1/clang+llvm-3.7.1-armv7a-linux-gnueabihf.tar.xz && \
      sudo tar xvf clang+llvm-3.7.1-armv7a-linux-gnueabihf.tar.xz -C /opt
      ```

      Run this now and add it to the `.profile`:

      ```
      export PATH="$HOME/.local/bin:/opt/clang+llvm-3.7.1-armv7a-linux-gnueabihf/bin:$PATH"
      ```

    * GHC 7.10.3

      ```
      wget http://llvm.org/releases/3.5.2/clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz && \
      sudo tar xvf clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz -C /opt
      ```

      Run this now and add it to the `.profile`:

      ```
      export PATH="$HOME/.local/bin:/opt/clang+llvm-3.5.2-armv7a-linux-gnueabihf/bin:$PATH"
      ```

12. Install Stack

    Binary: get an [existing `stack` binary](https://github.com/commercialhaskell/stack/releases)
and put it in `~/.local/bin`.

    From source (using cabal-install):

    ```
    wget http://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-armv7-deb8-linux.tar.xz && \
    tar xvf ghc-7.10.3-armv7-deb8-linux.tar.xz && \
    cd ghc-7.10.3 && \
    ./configure --prefix=/opt/ghc-7.10.3 && \
    sudo make install && \
    cd ..
    export PATH="/opt/ghc-7.10.3/bin:$PATH"
    wget https://www.haskell.org/cabal/release/cabal-install-1.24.0.0/cabal-install-1.24.0.0.tar.gz &&&&& \
    tar xvf cabal-install-1.24.0.0.tar.gz && \
    cd cabal-install-1.24.0.0 && \
    EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh && \
    cd .. && \
    export PATH="$HOME/.cabal/bin:$PATH" && \
    cabal update
    ```

    Edit `~/.cabal/config`, and set `executable-stripping: False` and `library-stripping: False`.

    ```
    cabal unpack stack && \
    cd stack-* && \
    cabal install && \
    mv ~/.cabal/bin/stack ~/.local/bin
    ```
