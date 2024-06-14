# Archive - from releases.md

## Build Linux static binary distribution with Nix

**NOTE: We have switched back to Alpine Linux for building static binaries, done by CI.  Leaving this section for future reference.**

These instructions are tested on Ubuntu 16.04, but theoretically should work on
any Linux distribution.

- Install nix (tested with v2.0.4 and v2.1.2, but should work with any)

  ~~~sh
  curl https://nixos.org/nix/install | sh
  ~~~

- Install and authenticate cachix (first two steps at https://cachix.org/ after
  signing up)


- Add nh2's cache:

    ~~~sh
    cachix use static-haskell-nix
    ~~~

  !!! note

      To clear cache index, use `rm $HOME/.cache/nix/binary-cache-v5.sqlite*`
      (useful if someone else uploads new stuff to the cache and you want to use
      it right away). The recent `narinfo-cache-positive`/`negative-ttl` options
      might also help.

- Check out Stack commit to be released to `~/stack-release` (or elsewhere, in
  which case adjust following instructions)

- `rm -f ~/stack-release/*.cabal`, to ensure it's regenerated

- clone https://github.com/nh2/static-haskell-nix recursively (last known to
  work with commit 725ceb2479637b3b3ab29298a1bc0e48c54984c9)

- in `static-stack` directory, run (from `static-stack/README.md`):

  ~~~sh
  $(nix-build --no-link -A run-stack2nix-and-static-build-script --argstr stackDir ~/stack-release)
  ~~~

- Run integration tests against the static binary [TODO: improve this process by
  adding full support in `release.hs` or the integration tests for testing a
  binary built elsewhere]

    - In `~/stack-release`, run
      `stack build --flag stack:integration-tests stack:stack-integration-test`
    - Copy binary built above to place where `stack build` normally puts the
      `stack binary` (e.g.
      `cp  /nix/store/7vl1xvlbbqjvf864inz5vw7z2z1k4nmw-stack-2.1.0.1/bin/stack /home/vagrant/stack-release/.stack-work/install/x86_64-linux/custom-snapshot-for-building-stack-with-ghc-8.2.2-PyNP5UoO8Ott/8.2.2/bin/stack`;
      figure it out using `stack exec which stack`)
    - Run `stack exec stack-integration-test`

- Copy the binary built above (in `/nix/store/XXX-stack-X.Y.Z/bin/stack`) to
  `~/stack-release/_release/bin/stack-X.Y.Z-linux-x86_64/stack` (replace `X.Y.Z`
  with the version, and the `/nix/store/*` path with that output at the end of
  the previous command)

- Package, sign, and upload to GitHub using Stack's release script in the stack
  directory:

  ~~~sh
  cd ~/stack-release
  stack etc/scripts/release.hs --no-test-haddocks --binary-variant=static --build-args=--dry-run upload
  ~~~

  (adding `--build-args=--dry-run` ensures the binary you copied will be used rather than building a new one)

- Download the bindist from GitHub and double check that the `stack` in it is
  actually static (use `ldd /path/to/stack`) and that `--version` reports
  correctly (and not dirty).

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
    * Enable 3D and 2D accelerated mode (this makes programs with lots of
      console output much faster)
    * Enabled shared clipboard (in VM window, Devices->Shared
      Clipboard->Both Directions)

Now continue to the **General Windows setup** subsection below.

## Using ESXi

1. Download the **MSEdge on Win10** VM for **VMWare (Windows, Mac)**.
2. Unzip the file downloaded file
3. Upload the VMDK file to the ESXi datastore
4. SSH into ESXi CLI and run:
    - `vmkfstools -i /vmfs/volumes/datastore1/win10-msedge/MSEdge-Win10-VMWare-disk1-ORIG.vmdk /vmfs/volumes/datastore1/win10-msedge/MSEdge-Win10-VMWare-disk1.vmdk -d thin`.
      This converts the disk to a format that is compatible with ESXi. You may
      have to run `esxcli system module load -m multiextent` first (see
      https://www.virtuallyghetto.com/2012/09/2gbsparse-disk-format-no-longer-working.html).
    - `vmkfstools -X 80G /vmfs/volumes/datastore1/win10-msedge/MSEdge-Win10-VMWare-disk1.vmdk`.
      This makes the disk twice as large, which helps avoid running out of disk
      space.
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
		- In all likelihood, you will want to search for "remote desktop" and enable
      remote desktop. Then you can connect to the VM using Microsoft Remote
      Desktop instead of using it from within the ESXi web UI.

Now continue to the **General Windows setup** subsection below.

## General Windows setup

 5. In **Settings**->**Update & Security**->**Windows Update**->**Advanced options**:
     * Change **Choose how updates are installed** to **Notify to schedule restart**
     * Check **Defer upgrades** (this avoids rebooting in the middle of the stack
       build)

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

    ~~~text
    md C:\p
    md C:\tmp
    cd /d C:\p
    ~~~

14. Create `C:\p\env.bat`:

    ~~~text
    SET TEMP=C:\tmp
    SET TMP=C:\tmp
    SET PATH=C:\Users\IEUser\AppData\Roaming\local\bin;"c:\Program Files\Git\usr\bin";"C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin";%PATH%
    ~~~

15. Run `C:\p\env.bat` (do this every time you open a new command prompt)

16. `stack exec -- gpg --import`, and paste in the your GPG secret key (must be
    done using `stack exec` because that uses the right keyring for the embedded
    MSYS2 GPG; you can get the key from another machine with
    `gpg --export-secret-keys --armor <KEY ID>`)

17. Run in command prompt (adjust the `user.email` and `user.name` settings):

    ~~~text
    git config --global user.email manny@fpcomplete.com
    git config --global user.name "Emanuel Borsboom"
    git config --global push.default simple
    git config --global core.autocrlf true
    git clone https://github.com/borsboom/stack-installer.git
    git clone -b stable --reference C:\p\stack-release https://github.com/commercialhaskell/stack.git stack-release
    cd stack-release
    stack install cabal-install
    ~~~

## Setting up an ARM VM for releases

1. Use Scaleway to start ARMv7 and ARM64 VMs.

2. Select Ubuntu Xenial as the operating system

3. Install the correct version of LLVM: `sudo apt-get install -y llvm-3.9`
   (appropriate for GHC 8.2, might need different version for other GHCs)

4. Symlink opt-3.X to `opt`: `sudo ln -s opt-3.9 /usr/bin/opt` (adjust the
   version if you installed a different one above)

5. Switch to gold linker:

    ~~~sh
    update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
    update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
    update-alternatives --config ld
    ~~~

6. Add swap space:

    ~~~sh
    dd if=/dev/zero of=/swapfile1 bs=1024 count=4194304
    mkswap /swapfile1
    swapon /swapfile1
    echo '/swapfile1 none swap sw 0 0' >>/etc/fstab
    ~~~

7. Install additional tools:

    ~~~Sh
    apt-get update && apt-get install -y unzip gpg
    ~~~

8. Import your GPG key (`gpg --import` and paste the private key)

9. Git settings (adjust for your preferences/email/name)

   ~~~text
   git config --global push.default simple
   git config --global user.email "manny@fpcomplete.com"
   git config --global user.name "Emanuel Borsboom"
   ~~~

10. Install tools used during building and dependencies packages

    ~~~text
    sudo apt-get install -y g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg
    ~~~

11. Install clang+llvm

    NOTE: the Debian jessie `llvm` package does not work (executables built with
    it just exit with "schedule: re-entered unsafely.").

    The version of LLVM needed depends on the version of GHC you need.

    * GHC 8.2.2 (the standard for building Stack)

      ~~~sh
      wget http://llvm.org/releases/3.9.1/clang+llvm-3.9.1-armv7a-linux-gnueabihf.tar.xz && \
      sudo tar xvf clang+llvm-3.9.1-armv7a-linux-gnueabihf.tar.xz -C /opt
      ~~~

      Run this now and add it to the `.profile`:

      ~~~sh
      export PATH="$HOME/.local/bin:/opt/clang+llvm-3.9.1-armv7a-linux-gnueabihf/bin:$PATH"
      ~~~

    * GHC 7.10.3

      ~~~sh
      wget http://llvm.org/releases/3.5.2/clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz && \
      sudo tar xvf clang+llvm-3.5.2-armv7a-linux-gnueabihf.tar.xz -C /opt
      ~~~

      Run this now and add it to the `.profile`:

      ~~~sh
      export PATH="$HOME/.local/bin:/opt/clang+llvm-3.5.2-armv7a-linux-gnueabihf/bin:$PATH"
      ~~~

12. Install Stack

    Binary: get an
    [existing `stack` binary](https://github.com/commercialhaskell/stack/releases)
    and put it in `~/.local/bin`.

    From source, using Cabal (the tool):

    ~~~sh
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
    ~~~

    Edit `~/.cabal/config`, and set `executable-stripping: False` and
    `library-stripping: False`.

    ~~~sh
    cabal unpack stack && \
    cd stack-* && \
    cabal install && \
    mv ~/.cabal/bin/stack ~/.local/bin
    ~~~
