# Maintainer guide

## Next release:

## Pre-release steps

* Ensure `release` and `stable` branches merged to `master`
* Check compatibility with latest nightly stackage snapshot:
    * Update `stack-nightly.yaml` with latest nightly and remove extra-deps
    * Run `stack --stack-yaml=stack-nightly.yaml test`
* Ensure integration tests pass on a representative Windows, macOS, and Linux (Linux
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
    * Update the ChangeLog:
        * Check for any important changes that missed getting an entry in
          Changelog (`git log origin/stable...HEAD`)
        * Check for any entries that snuck into the previous version's changes
          due to merges (`git diff origin/stable HEAD ChangeLog.md`)
    * Review documentation for any changes that need to be made
        * Search for old Stack version, unstable stack version, and the next
          "obvious" version in sequence (if doing a non-obvious jump), and
          `UNRELEASED` and replace with new version
        * Look for any links to "latest" documentation, replace with version tag
        * Ensure all documentation pages listed in `mkdocs.yaml`
    * Update the ISSUE_TEMPLATE.md to point at the new version.
    * (SKIP) Check that any new Linux distribution versions added to
      `etc/scripts/release.hs` and `etc/scripts/vagrant-releases.sh`
        * [Ubuntu](https://wiki.ubuntu.com/Releases)
        * [Debian](https://www.debian.org/releases/)
        * [CentOS](https://wiki.centos.org/Download)
        * [Fedora](https://fedoraproject.org/wiki/Releases)
    * Check for new [FreeBSD release](https://www.freebsd.org/releases/).
    * Check that no new entries need to be added to
      [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
      [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),
      and
      `README.md`
    * Remove unsupported/obsolete distribution versions from the release process.
        * [Ubuntu](https://wiki.ubuntu.com/Releases)
            * 12.04 EOL 2017-APR
            * 16.10 EOL 2017-JUL
            * 14.04 EOL 2019-APR
            * 16.04 EOL 2021-APR
        * [Debian](https://www.debian.org/releases/)
        * [CentOS](https://wiki.centos.org/Download) 
            * 6 EOL 2020-NOV-30
            * 7 EOL 2024-JUN-30

## Release process

See
[stack-release-script's README](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/README.md#prerequisites)
for requirements to perform the release, and more details about the tool.

A note about the `etc/scripts/*-releases.sh` scripts: if you run them from a
different working tree than the scripts themselves (e.g. if you have `stack1`
and `stack2` trees, and run `cd stack1;
../stack2/etc/scripts/vagrant-release.sh`) the scripts and Vagrantfiles from the
tree containing the script will be used to build the stack code in the current
directory. That allows you to iterate on the release process while building a
consistent and clean stack version.

* Create a
  [new draft Github release](https://github.com/commercialhaskell/stack/releases/new)
  with tag and name `vX.Y.Z` (where X.Y.Z is the stack package's version), targetting the
  RC branch

* On each machine you'll be releasing from, set environment variables:
  `GITHUB_AUTHORIZATION_TOKEN`, `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
  `AWS_DEFAULT_REGION`.

    Note: since one of the tools (rpm-s3 on CentOS) doesn't support AWS
    temporary credentials, you can't use MFA with the AWS credentials
    (`AWS_SECURITY_TOKEN` is ignored).

* On a machine with Vagrant installed:
    * Run `etc/scripts/vagrant-releases.sh`

* On macOS:
    * Run `etc/scripts/osx-release.sh`

* On Windows:
    * Ensure your working tree is in `C:\stack` (or a similarly short path)
    * Run `etc\scripts\windows-releases.bat`
    * Release Windows installers. See
      [stack-installer README](https://github.com/borsboom/stack-installer#readme)

* On Linux ARMv7:
    * Run `etc/scripts/linux-armv7-release.sh`

* Build sdist using `stack sdist . --pvp-bounds=both`, and upload it to the
  Github release with a name like `stack-X.Y.Z-sdist-0.tar.gz`.

* Publish Github release. Use e.g. `git shortlog -s v1.1.2..rc/v1.2.0|sed
  's/^[0-9 ]*/* /'|sort -f` to get the list of contributors.

* Upload package to Hackage: `stack upload . --pvp-bounds=both`

* On a machine with Vagrant installed:
    * Run `etc/scripts/vagrant-distros.sh`

* (SKIP) Submit a PR for the
  [haskell-stack Homebrew formula](https://github.com/Homebrew/homebrew-core/blob/master/Formula/haskell-stack.rb)
      * Ensure that the formula use the sdist uploaded to the Github release
      * Be sure to update the SHA sum
      * The commit message should just be `haskell-stack <VERSION>`

* (SKIP) [Flag the Arch Linux package as out-of-date](https://www.archlinux.org/packages/community/x86_64/stack/flag/)

* Push signed Git tag, matching Github release tag name, e.g.: `git tag -d vX.Y.Z; git tag -u
  0x575159689BEFB442 vX.Y.Z && git push -f origin vX.Y.Z`

* Reset the `release` branch to the released commit, e.g.: `git checkout release
  && git merge --ff-only vX.Y.Z && git push origin release`

* Update the `stable` branch similarly

* Delete the RC branch (locally and on origin)

* Activate version for new release tag on
  [readthedocs.org](https://readthedocs.org/dashboard/stack/versions/), and
  ensure that stable documentation has updated

* Upload haddocks to Hackage: `etc/scripts/upload-haddocks.sh`

* Merge any changes made in the RC/release/stable branches to master.

* Announce to haskell-cafe@haskell.org, haskell-stack@googlegroups.com,
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

 5. In **Settings**->**Update & Security**->**Windows Update**->**Advanced options**:
     * Change **Choose how updates are installed** to **Notify to schedule restart**
     * Check **Defer upgrades**

 6. Configure a shared folder for your home directory on the host, and mount it on Z:

 7. Install Windows SDK (for signtool):
    http://microsoft.com/en-us/download/confirmation.aspx?id=8279

 8. Install msysgit: https://msysgit.github.io/

 9. Install nsis-2.46.5-Unicode-setup.exe from http://www.scratchpaper.com/

10: Install Stack using the Windows 64-bit installer

11. Visit https://hackage.haskell.org/ in Edge to ensure system has correct CA
    certificates

12. Get the object code certificate from
    [password-store](https://github.com/fpco/password-store), in
    `certificates/code_signing/fpcomplete_corporation_startssl_2015-09-22.pfx`.
    Double click it in explorer and import it

13. Run in command prompt:

        md C:\p
        md C:\p\tmp
        cd \p
        md c:\tmp

14. Create `C:\p\env.bat`:

        SET STACK_ROOT=C:\p\.sr
        SET TEMP=C:\p\tmp
        SET TMP=C:\p\tmp
        SET PATH=C:\Users\IEUser\AppData\Roaming\local\bin;"c:\Program Files\Git\usr\bin";"C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin";%PATH%

15. Run `C:\p\env.bat` (do this every time you open a new command prompt)

16. Import the `dev@fpcomplete.com` (0x575159689BEFB442) GPG secret key

17. Run in command prompt (adjust the `user.email` and `user.name` settings):

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

## Setting up an ARM VM for releases

These instructions assume the host system is running macOS. Some steps will vary
with a different host OS.

### Install qemu on host

    brew install qemu

### Install fuse-ext2

    brew install e2fsprogs m4 automake autoconf libtool && \
    git clone https://github.com/alperakcan/fuse-ext2.git && \
    cd fuse-ext2 && \

Add `m4_ifdef([AM_PROG_AR], [AM_PROG_AR])` to the `configure.ac` after
`m4_ifdef([AC_PROG_LIB],[AC_PROG_LIB],[m4_warn(portability,[Missing AC_PROJ_LIB])])`
line.

    PKG_CONFIG_PATH="$(brew --prefix e2fsprogs)/lib/pkgconfig" \
        CFLAGS="-idirafter/$(brew --prefix e2fsprogs)/include -idirafter/usr/local/include/osxfuse" \
        LDFLAGS="-L$(brew --prefix e2fsprogs)/lib" \
        ./configure

### Create VM and install Debian in it

    wget http://ftp.de.debian.org/debian/dists/jessie/main/installer-armhf/current/images/netboot/initrd.gz && \
    wget http://ftp.de.debian.org/debian/dists/jessie/main/installer-armhf/current/images/netboot/vmlinuz && \
    wget http://ftp.de.debian.org/debian/dists/jessie/main/installer-armhf/current/images/device-tree/vexpress-v2p-ca9.dtb && \
    qemu-img create -f raw armdisk.raw 15G && \
    qemu-system-arm -M vexpress-a9 -cpu cortex-a9 -kernel vmlinuz -initrd initrd.gz -sd armdisk.raw -append "root=/dev/mmcblk0p2" -m 1024M -redir tcp:2223::22 -dtb vexpress-v2p-ca9.dtb -append "console=ttyAMA0,115200" -serial stdio

Now the Debian installer will run. Don't use LVM for partitioning (it won't
BOOT), and add at least 2 GB swap during installation.

### Get boot files after install

    hdiutil attach -imagekey diskimage-class=CRawDiskImage -nomount armdisk.raw && \
    mkdir -p /Volumes/armdeb && \
    fuse-ext2 /dev/disk2s1 /Volumes/armdeb/ && \
    sleep 5 && \
    cp /Volumes/armdeb/vmlinuz-3.16.0-4-armmp . && \
    cp /Volumes/armdeb/initrd.img-3.16.0-4-armmp . && \
    hdiutil detach /dev/disk2

### Boot VM

    qemu-system-arm -M vexpress-a9 -cpu cortex-a9 -kernel vmlinuz-3.16.0-4-armmp -initrd initrd.img-3.16.0-4-armmp -sd armdisk.raw -m 1024M -dtb vexpress-v2p-ca9.dtb -append "root=/dev/mmcblk0p2 console=ttyAMA0,115200" -serial stdio -redir tcp:2223::22

### Setup rest of system

Log onto the VM as root, then (replace `<<<USERNAME>>>` with the user you set up
during Debian installation):

    apt-get update && \
    apt-get install -y sudo && \
    adduser <<<USERNAME>>> sudo

Now you can SSH to the VM using `ssh -p 2223 <<<USERNAME>>>@localhost` and use `sudo` in
the shell.

### Install clang+llvmGHC/clang

NOTE: the Debian jessie `llvm` packge does not work (executables built with it
just exit with "schedule: re-entered unsafely.").

The version of LLVM needed depends on the version of GHC you need.

#### GHC 7.10.3 (the standard for building Stack)

    sudo apt-get install -y g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg && \
    wget http://llvm.org/releases/3.5.2/clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz && \
    sudo tar xvf clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz -C /opt

Run this now and add it to the `.profile`:

    export PATH="$HOME/.local/bin:/opt/clang+llvm-3.5.2-armv7a-linux-gnueabihf/bin:$PATH"

#### GHC 8.0.1

    wget http://llvm.org/releases/3.7.1/clang+llvm-3.7.1-armv7a-linux-gnueabihf.tar.xz && \
    sudo tar xvf clang+llvm-3.7.1-armv7a-linux-gnueabihf.tar.xz -C /opt

Run this now and add it to the `.profile`:

    export PATH="$HOME/.local/bin:/opt/clang+llvm-3.5.2-armv7a-linux-gnueabihf/bin:$PATH"

### Install Stack

#### Binary

Get an [existing `stack` binary](https://github.com/commercialhaskell/stack/releases)
and put it in `~/.local/bin`.

#### From source (using cabal-install):

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

Edit `~/.cabal/config`, and set `executable-stripping: False` and
`library-stripping: False`.

    cabal unpack stack && \
    cd stack-* && \
    cabal install && \
    mv ~/.cabal/bin/stack ~/.local/bin

### Resources

  - http://mashu.github.io/2015/08/12/QEMU-Debian-armhf.html
  - https://www.aurel32.net/info/debian_arm_qemu.php
  - http://linuxdeveloper.blogspot.ca/2011/08/how-to-install-arm-debian-on-ubuntu.html
  - http://www.macworld.com/article/2855038/how-to-mount-and-manage-non-native-file-systems-in-os-x-with-fuse.html
  - https://github.com/alperakcan/fuse-ext2#mac-os
  - https://github.com/alperakcan/fuse-ext2/issues/31#issuecomment-214713801
  - https://github.com/alperakcan/fuse-ext2/issues/33#issuecomment-216758378
  - https://github.com/alperakcan/fuse-ext2/issues/32#issuecomment-216758019
  - http://osxdaily.com/2007/03/23/create-a-ram-disk-in-mac-os-x/

## Adding a new GHC version

  * Push new tag to our fork:

        git clone git@github.com:commercialhaskell/ghc.git
        cd ghc
        git remote add upstream git@github.com:ghc/ghc.git
        git fetch upstream
        git push origin ghc-X.Y.Z-release

  * [Publish a new Github release](https://github.com/commercialhaskell/ghc/releases/new)
    with tag `ghc-X.Y.Z-release` and same name.

  * Down all the relevant GHC bindists from https://www.haskell.org/ghc/download_ghc_X_Y_Z and upload them to the just-created Github release (see
    [stack-setup-2.yaml](https://github.com/fpco/stackage-content/blob/master/stack/stack-setup-2.yaml)
    for the ones we used in the last GHC release).

    In the case of macOS, repackage the `.xz` bindist as a `.bz2`, since macOS does
    not include `xz` by default or provide an easy way to install it.

  * Build any additional required bindists (see below for instructions)

      * tinfo6 (`etc/vagrant/fedora24-x86_64`)
      * ncurses6 (`etc/vagrant/arch-x86_64`)

  * [Edit stack-setup-2.yaml](https://github.com/fpco/stackage-content/edit/master/stack/stack-setup-2.yaml)
    and add the new bindists, pointing to the Github release version. Be sure to
    update the `content-length` and `sha1` values.

### Building GHC

On systems with a small `/tmp`, you should set TMP and TEMP to an alternate
location.

For GHC >= 7.10.2, set the `GHC_VERSION` environment variable to the version to build:

    * `export GHC_VERSION=8.0.1`
    * `export GHC_VERSION=7.10.3a`
    * `export GHC_VERSION=7.10.2`

then, run (from [here](https://ghc.haskell.org/trac/ghc/wiki/Newcomers)):

    git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/ && \
    git clone -b ghc-${GHC_VERSION}-release --recursive git://github.com/ghc/ghc ghc-${GHC_VERSION} && \
    cd ghc-${GHC_VERSION}/ && \
    cp mk/build.mk.sample mk/build.mk && \
    sed -i 's/^#BuildFlavour *= *perf$/BuildFlavour = perf/' mk/build.mk && \
    ./boot && \
    ./configure --enable-tarballs-autodownload && \
    sed -i 's/^TAR_COMP *= *bzip2$/TAR_COMP = xz/' mk/config.mk && \
    make -j$(cat /proc/cpuinfo|grep processor|wc -l) && \
    make binary-dist

GHC 7.8.4 is slightly different:

    export GHC_VERSION=7.8.4 && \
    git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/ && \
    git clone -b ghc-${GHC_VERSION}-release --recursive git://github.com/ghc/ghc ghc-${GHC_VERSION} && \
    cd ghc-${GHC_VERSION}/ && \
    ./sync-all --extra --nofib -r git://git.haskell.org get -b ghc-7.8 && \
    cp mk/build.mk.sample mk/build.mk && \
    sed -i 's/^#BuildFlavour *= *perf$/BuildFlavour = perf/' mk/build.mk && \
    perl boot && \
    ./configure && \
    sed -i 's/^TAR_COMP *= *bzip2$/TAR_COMP = xz/' mk/config.mk && \
    make -j$(cat /proc/cpuinfo|grep processor|wc -l) && \
    make binary-dist
