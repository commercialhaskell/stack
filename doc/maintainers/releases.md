<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Releases

## Upcoming release tasks:

* @nh2 says: "For the next static stack release we need to have unix-compat >= 0.5.2, because 0.5.1 breaks with musl (because it tries to use something as a symbol that is a C macro).  https://github.com/nh2/static-haskell-nix/issues/79  In my CI I've just bumped unix-compat accordingly: https://github.com/nh2/static-haskell-nix/pull/80/files"
* Eventually remove the Ubuntu, Debian, CentOS, Arch packages from our S3 bucket.  This was announced with the 1.9.x release, so can do this around time of 2.1.x.  Directories, and last Stack version uploaded:
	- `s3://download.fpcomplete.com/archlinux` (1.0.0)
	- `s3://download.fpcomplete.com/centos` (1.5.1)
	- `s3://download.fpcomplete.com/debian` (1.4.0)
	- `s3://download.fpcomplete.com/fedora` (1.3.0)
	- `s3://download.fpcomplete.com/ubuntu` (1.5.1)
* Look through https://fpcomplete.slack.com/files/U9U8HDGUC/FCM7UN5NJ/notes_on_doc_maintainers_releases_md.txt for hints on how to make this document more clear.
* If `store` is no longer a dependency, likely can remove from stackage build constraints' `expected-test-failures`
* Try using Alpine and https://github.com/redneb/ghc-alt-libc/releases to build linux 32-bit static binaries.  This will require upgrading GHC version used (e.g. to 8.6.5).  If this works out, switch over to the same for 64-bit static binaries (rather than static-haskell-nix).
* Fix the reference to "latest nightly stackage snapshot" below to work with a new workflow based on the GHC-named stack.yaml files.

## Iterating on release process

**IMPORTANT: all bindists for a given release should be built from a consistent git commit** (this can be relaxed a bit for release candidates, but still should be maintained as much as possible).

Since the release process and scripts sometimes need to be iterated on during the process of building all the platforms' binaries, the scripts are all designed so that you can run them *from a different directory*.  This means you can have one source tree for the version of `stack` being released, and a separate one where you work on the release scripts and process.

To use this way, the current directory should be the version of `stack` to be released.  You can then call the scripts in another directory to use their version.  For example, `stack ../stack-release-scripts/etc/scripts/release.hs …` or `../stack-release-scripts/etc/scripts/vagrant-releases.hs`.


## Version scheme

* Versions with an _even_ second component are development versions (the `master` branch)
* Versions with an _odd_ second component are stable versions (the `stable` branch, or in a `rc/vX.Y` release candidate branch for not-yet-released versions)
* Versions with an _even_ third component (e.g. 1.6.2 and 1.7.0) are unreleased versions
* Versions with an _odd_ third component (e.g. 1.6.1 or 1.7.3) and released versions
* Pre-release unstable binaries will be released with the date as the fourth component (e.g. 1.6.0.20171129)
* Release candidate binaries will be released with an even third component and and odd number as the fourth component (e.g. 1.7.0.1)
* Hackage-only dependency compatibility patch releases add a fourth patchlevel component (e.g. v1.7.3.1, in the `release` branch)
* All branches _except_ `release` (which matches exactly the most recent release) must have an even third component (development)
* Branches other than `stable`, `release`, and a `rc/vX.Y` release candidate will always have a `0` third component (e.g. 1.7.0).

Examples:

* `1.7.0.0`: v1.7.x series pre-release branch (`v1.7` branch)
* `1.7.0.1`: release candidate for first release of v1.7.x series (`v1.7` branch)
* `1.7.0.2`: continuing development on pre-release branch
* `1.7.0.3`: second release candidate for first release of v1.7.x series (`v1.7` branch)
* `1.7.1`: first release of the 1.7.x series (`release` branch)
* `1.7.2.0`: development for second release of 1.7.x series (`stable` branch)
* `1.7.2.1`: release candidate for second release of 1.7.x series (`stable` branch)
* `1.7.3`: second release of 1.7.x series (`release` branch)
* `1.7.3.1`: first hackage-only patch of 1.7.3 (`release` branch)
* `1.7.3.2`: second hackage-only patch of 1.7.3 (`release` branch)
* `1.8.0`: unstable development code (`master` branch)
* `1.8.0.20181004`: pre-release snapshot of unstable version (`master` branch)

## Pre-release checks

* Check that the snapshot in `stack.yaml`'s GHC version supports building on all required platforms (e.g. GHC 8.4 doesn't support FreeBSD, so it's not a candidate).  If not, will have to switch to a different snapshot (or decide that we won't support the platform for this iteration).
* Check for any P0 and P1 issues that should be dealt with before release
* Check for un-merged pull requests that should be merged before release
* Ensure `release` and `stable` branches merged to `master`
* Ensure no bounds specified in any package.yaml unless truly necessary (e.g. in most cases rely on stacksnap.yaml to manage dependency versions), and ensure `base` minbound is correct for the supported GHC version(s) (check version of `base` in oldest supported Stackage snapshot)
* Check compatibility with latest LTS Stackage snapshots
    * `snapshot*.yaml` and `stack*.yaml` (where `*` is not `nightly`), __including the ones in
      subdirectories__: bump to use latest LTS minor
      version (be sure any extra-deps that exist only for custom flags have
      versions matching the snapshot)
    * Check for any redundant extra-deps
    * Run `stack --stack-yaml=stack*.yaml test --pedantic` (replace `*` with
      the actual file)
* Check compatibility with latest nightly stackage snapshot:
    * Update `snapshot-nightly.yaml` with latest nightly and remove unnecessary extra-deps (be
      sure any extra-deps that exist only for custom flags have versions
      matching the snapshot)
    * Run `stack --stack-yaml=stack-nightly.yaml test --pedantic`
* Update (increase lower bound) or remove (if redundant) any constraints in `package.yaml`
* Update `.azure-pipelines*.yml` and files in `.azure` for any GHC version changes made in above steps.
* Ensure CI matrices in docs (travis-complex, appveyor, azure) have current stackage snapshots and GHC versions (e.g. https://github.com/commercialhaskell/stack/pull/4565/files)
* Ensure integration tests pass on a Windows, macOS, and Linux.  Do so by checking that the latest nightly build for the `master` branch succeeded in Azure DevOps (or kick one off manually if any significant changes were made since the last automated build).

## Release preparation

* In master branch:
    * `package.yaml`: bump to next release candidate version (bump second component to next odd number, ensure third component is `0`, and add patchlevel `0`; e.g. from `1.8.0` to `1.9.0.0`)
    * `ChangeLog.md`
        * Check for any entries that snuck into the previous version's changes
          due to merges (`git diff origin/stable HEAD ChangeLog.md`)

* Cut a release candidate branch `rc/vX.Y` from master

* In master branch:
    * `package.yaml`: bump version to next unstable version (next even second component with `.0` third component (e.g. from 1.9.0 to 1.10.0)
    * `Changelog.md`:
      * Change the title of the existing **Unreleased changes** section to what will be the next final (non-RC) release (e.g. `v2.1.1`).
      * add new "Unreleased changes" section:
        ```
        ## Unreleased changes

        **Changes since vX.Y.Z**

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
          * Do **NOT** update the Dockerfiles in [stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/) yet; that will come later)
            * Do __NOT__ update templates in `.github` to point at the new release version yet!
        * Search for old resolvers, set to latest resolver (e.g. in `doc/GUIDE.md` where it references the "currently the latest LTS")
        * Look for any links to "latest" (`latest/`) documentation, replace with version tag
    * Update `STACK_VERSION` in `etc/scripts/get-stack.sh` to the new release version (`X.Y.1`)
    * Check if GHC version we're using has bindists using upgraded versions of operating systems (e.g. FreeBSD, Debian) and upgrade relevant Vagrantfiles in `etc/vagrant` and release shell script (`etc/scripts/*-release.sh`) to match.
    * Check that for any platform entries that need to be added to (or removed from)
      [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
      [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md), [get-stack.sh](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/get-stack.sh), and [doc/README.md](https://github.com/commercialhaskell/stack/blob/master/doc/README.md).
    * `package.yaml`: bump to next release candidate version (bump patchlevel (fourth) component to next odd number; e.g. from `1.9.0.0` to `1.9.0.1`)
    * `ChangeLog.md`
        - Rename the “Unreleased changes” section to the same version as package.yaml, and mark it clearly as a release candidate (e.g. `v1.9.0.1 (release candidate)`).  Remove any empty sections.

* For first release candidate:

    * Re-do the pre-release checks (above section)
    * `package.yaml`: bump to first odd patchlevel version (e.g. `X.Y.0.1`)
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

* Manually trigger the nightly pipeline on Azure Devops for the branch you are releasing, which will build Linux, macOS, and Windows bindists. `[RC]`

* Create a
  [new draft Github release](https://github.com/commercialhaskell/stack/releases/new)
  with tag and title `vX.Y.Z` (where X.Y.Z matches the version in `package.yaml` from the previous step), targeting the RC branch.  In the case of a release candidate, add `(RELEASE CANDIDATE)` to the name field and check the *This is a pre-release* checkbox.  `[RC]`

* On each machine you'll be releasing from, set environment variables `GITHUB_AUTHORIZATION_TOKEN` and `STACK_RELEASE_GPG_KEY` (see [stack-release-script's README](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/README.md#prerequisites)). `[RC]`

* Upload the Azure bindists built by Azure Pipelines to the Gitlab release `[RC]` (TODO: integrate this into `release.hs`)
  * Download the bindist artifacts from the Azure DevOps nightly pipeline build, and put the contents under `_release/`.
  * For each file, run `stack "$(dirname "$0")/release.hs" --upload-only "_releases/<file>.upload" "_releases/<file>.sha256.upload" "_releases/<file>.asc.upload"` to sign, hash, and upload to Github

* On a machine with Vagrant installed:

    * Run `etc/scripts/vagrant-releases.sh`

* On Windows (TODO: add support for building/uploading installers to `release.hs` and the nightly CI jobs)

    * Use a short path for your working tree (e.g. `C:\p\stack-release`
    * Ensure that STACK_ROOT, TEMP, and TMP are set to short paths
    * Put the Azure Devops pipeline-built Windows binaries in `SOME-OTHER-DIRECTORY/_release`.
    * Build Windows installers. See
      [stack-installer README](https://github.com/borsboom/stack-installer#readme).

<!-- * On Linux ARMv7:

    * Run `etc/scripts/linux-armv7-release.sh` -->

* On Linux ARM64 (aarch64):

    * Run `etc/scripts/linux-aarch64-release.sh`

* Build a Linux static bindist `[RC]`

    * Follow directions in the **Build Linux static binary distribution with Nix** section below.

* For any GPG key used to sign an uploaded bindist, ensure that `dev@fpcomplete.com` signs their key and uploads to SKS keyserver pool:

  ```
  gpg --sign-key -u 0x575159689BEFB442 <OTHER-KEY-ID>
  gpg --send-keys <OTHER-KEY-ID>
  ```

* Publish Github release. Include the Changelog and in the description and use e.g. `git shortlog -s origin/release..HEAD|sed $'s/^[0-9 \t]*/* /'|grep -v azure-pipelines|LC_ALL=C sort -f` to get the list of contributors (contributors not necessary for release candidates). See previous releases for example formatting and extra info (such as link to website for install instructions).  `[RC]` (For release candidates, you can skip the list of contributors).

* Push signed Git tag, matching Github release tag name, e.g.: `git tag -d vX.Y.Z; git tag -s -m vX.Y.Z vX.Y.Z && git push -f origin vX.Y.Z`.  `[RC]`

* Upload `stack` package to Hackage: `stack upload . --pvp-bounds=lower`.


* Reset the `release` branch to the released commit, e.g.: `git checkout release && git merge --ff-only vX.Y.Z && git push origin release`

* Update the `stable` branch similarly

* Activate version for new release tag, on
  [readthedocs.org](https://readthedocs.org/dashboard/stack/versions/), and
  ensure that stable documentation has updated.

* Update [get.haskellstack.org /stable rewrite rules](https://gitlab.fpcomplete.com/fpco/devops/blob/master/dockerfiles/nginx/prod-v2/etc/nginx/conf.d/haskellstack.conf) (be sure to change both places) to new released version, and update production cluster using the `deploy_nginx_prod_v2` job.

    * Test with `curl -vL https://get.haskellstack.org/stable/linux-x86_64-static.tar.gz >/dev/null`, make sure it redirects to the new version

* In the `stable` or, in the case of a release candidate, `rc/vX.Y` branch:
    - `package.yaml`: bump the version number even third component (e.g. from 1.6.1 to 1.6.2) or, in the case of a release candidate even _fourth_ component (e.g. from 1.7.0.1 to 1.7.0.2). `[RC]`

    - `ChangeLog.md`: Add an “Unreleased changes” section (update “changes since” version):`[RC]`

      ```
      ## Unreleased changes

      **Changes since vX.Y.Z**

      Release notes:

      Major changes:

      Behavior changes:

      Other enhancements:

      Bug fixes:

      ```

    - Update templates in `.github` to point at the new release version (`X.Y.1`).  **SKIP THIS IN RC BRANCHES.**

    - Update fpco/stack-build Docker images with new version

      * Under [stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/), add `lts-X.Y/Dockerfile` (where `X.Y` is the latest stackage LTS version), containing (note where X.Z is the previous LTS version, and X.Y.Z is the newly released stack version)

        ```
        FROM fpco/stack-build:lts-X.Z
        ARG STACK_VERSION=X.Y.Z
        RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
        ```

      * Run `./build.sh lts-X.Y` and test that the new image has the new version of Stack.

      * Run `./build.sh --push lts-X.Y && ./build.sh --push --small lts-X.Y` to push the new image to the registry.


* Delete the RC branch (locally and on origin).  E.g. `git branch -d rc/vX.Y; git push origin :rc/vX.Y`.

* Merge any changes made in the RC/release/stable branches to master (be careful about version and changelog).   It is best to do this by making a `ci/merge-stable-to-master` branch and waiting for CI to pass, then merging.  If anything is complicated to merge, consider making it a PR and getting it reviewed rather than merging immediately.

* Announce to haskell-cafe@haskell.org, haskell-stack@googlegroups.com,
  commercialhaskell@googlegroups.com mailing lists, subject `ANN: stack-X.Y.Z` (or `ANN: stack-X.Y release candidate`), containing the markdown for the release description from Github. `[RC]`

* Add back to Stackage nightly if fallen out (be sure to have a `< 9.9.9` constraint to avoid the accidentally uploaded stack-9.9.9 from being used).


## Build Linux static binary distribution with Nix

TODO: script this process and/or integrate with `etc/scripts/release.hs`. 
NOTE: With GHC 8.8.x, Alpine Linux GHC bindists are once again available and we may be able to switch back to use Alpine to build static binaries using out normal build/release process rather than this convoluted process.

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

- `rm -f ~/stack-release/*.cabal`, to ensure it's regenerated

- clone https://github.com/nh2/static-haskell-nix recursively (last known to work with commit 725ceb2479637b3b3ab29298a1bc0e48c54984c9)

- in `static-stack` directory, run (from `static-stack/README.md`):

  ```
  $(nix-build --no-link -A run-stack2nix-and-static-build-script --argstr stackDir ~/stack-release)
  ```

- Run integration tests against the static binary [TODO: improve this process by adding full support in `release.hs` or the integration tests for testing a binary built elsewhere]

    - In `~/stack-release`, run `stack build --flag stack:integration-tests stack:stack-integration-test`
    - Copy binary built above to place where `stack build` normally puts the `stack binary` (e.g. `cp  /nix/store/7vl1xvlbbqjvf864inz5vw7z2z1k4nmw-stack-2.1.0.1/bin/stack /home/vagrant/stack-release/.stack-work/install/x86_64-linux/custom-snapshot-for-building-stack-with-ghc-8.2.2-PyNP5UoO8Ott/8.2.2/bin/stack`; figure it out using `stack exec which stack`)
    - Run `stack exec stack-integration-test`

- Copy the binary built above (in `/nix/store/XXX-stack-X.Y.Z/bin/stack`) to `~/stack-release/_release/bin/stack-X.Y.Z-linux-x86_64-static/stack` (replace `X.Y.Z` with the version, and the `/nix/store/*` path with that output at the end of the previous command)

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
		- In all likelihood, you will want to search for "remote desktop" and enable remote desktop.  Then you can connect to the VM using Microsoft Remote Desktop instead of using it from within the ESXi web UI.

Now continue to the **General Windows setup** subsection below.

## General Windows setup

 5. In **Settings**->**Update & Security**->**Windows Update**->**Advanced options**:
     * Change **Choose how updates are installed** to **Notify to schedule restart**
     * Check **Defer upgrades** (this avoids rebooting in the middle of the stack build)

 6. In **Settings**->**System**->**Power & sleep**

    * Disable turning off the screen or going to sleep when plugged in

 7. Install msysgit: https://msysgit.github.io/

 8. Install TortoiseHG: https://tortoisehg.bitbucket.io/download/index.html

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
        git clone https://github.com/borsboom/stack-installer.git
        git clone -b stable --reference C:\p\stack-release https://github.com/commercialhaskell/stack.git stack-release
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

    * GHC 8.2.2 (the standard for building Stack)

      ```
      wget http://llvm.org/releases/3.9.1/clang+llvm-3.9.1-armv7a-linux-gnueabihf.tar.xz && \
      sudo tar xvf clang+llvm-3.9.1-armv7a-linux-gnueabihf.tar.xz -C /opt
      ```

      Run this now and add it to the `.profile`:

      ```
      export PATH="$HOME/.local/bin:/opt/clang+llvm-3.9.1-armv7a-linux-gnueabihf/bin:$PATH"
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
