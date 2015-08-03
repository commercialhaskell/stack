Distribution packages are available for [Ubuntu](#ubuntu), [Debian](#debian), [CentOS / Red Hat](#centos--red-hat), [Fedora](#fedora) and [Arch Linux](#arch-linux). Binaries for other operating systems are available on [the releases page](https://github.com/fpco/stack/releases). For the future, we have plans to support more OSes.

Binaries are signed with this [[Signing key]].

## Windows

* Download [the latest release](https://github.com/commercialhaskell/stack/releases/latest). Note: while generally i386/32-bit GHC is better tested on Windows, there are reports that recent versions of Windows only work with the 64-bit version of stack (see [issue #393](https://github.com/commercialhaskell/stack/issues/393)).
* Unpack the `stack.exe` to somewhere on your `%PATH%` (see [Path section below](#path)) and you can then run `stack` on the commandline.

NOTE: These executables have been built and tested on a Windows 8.1 64-bit machine. They should run on older Windows installs as well, but have not been tested. If you do test, please edit and update this page to indicate as such.

## OS X

* Download [the latest release](https://github.com/commercialhaskell/stack/releases/latest)
* Extract the `stack` executable and put it somewhere on your `$PATH` (see [Path section below](#path))

We generally test on the current version of OS X, but stack is known to work on Mavericks as well, and may also work on older versions (YMMV).

**Note**: due to [GHC bug 10322](https://ghc.haskell.org/trac/ghc/ticket/10322), building `stack` from source fails with GHC 7.10.1. This bug is fixed in GHC 7.10.2, and GHC 7.8.4 works as well.

## Ubuntu

*note*: for 32-bit, use the [generic Linux option](#linux)

1. Get the FP Complete key:

        wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -

2. Add the appropriate source repository:

    * Ubuntu 15.04 (amd64):

            echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Ubuntu 14.10 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu/utopic stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Ubuntu 14.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Ubuntu 12.04 (amd64)

            echo 'deb http://download.fpcomplete.com/ubuntu/precise stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## Debian

*note*: for 32-bit, use the [generic Linux option](#linux)

1. Get the FP Complete key:

        wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/debian/fpco.key | sudo apt-key add -

2. Add the appropriate source repository:

    * Debian 8 (amd64):

            echo 'deb http://download.fpcomplete.com/debian/jessie stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

    * Debian 7 (amd64)

            echo 'deb http://download.fpcomplete.com/debian/wheezy stable main'|sudo tee /etc/apt/sources.list.d/fpco.list

3. Update apt and install

        sudo apt-get update && sudo apt-get install stack -y

## CentOS / Red Hat

*note*: for 32-bit, use the [generic Linux option](#linux)

1. Add the appropriate source repository:

    * CentOS 7 / RHEL 7 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

    Looking for CentOS 6 / RHEL 6?  See [#465](https://github.com/commercialhaskell/stack/issues/465#issuecomment-118844397).

2. Install:

        sudo yum -y install stack

## Fedora

*note*: for 32-bit, use the [generic Linux option](#linux)

1. Add the appropriate source repository:

    * Fedora 22 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/22/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

    * Fedora 21 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/21/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

    * Fedora 20 (x86_64)

            curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/20/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

2. Install:

    * Fedora 22+

            sudo dnf -y install stack

    * Fedora < 22

            sudo yum -y install stack

## Arch Linux

*note*: for 32-bit, use the [generic Linux option](#linux)

stack can be found in the AUR:
  - [haskell-stack](https://aur.archlinux.org/packages/haskell-stack/) _latest stable version_
  - [haskell-stack-git](https://aur4.archlinux.org/packages/haskell-stack-git/) _git version_

In order to install stack from Hackage or from source, you will need the [libtinfo](https://aur4.archlinux.org/packages/libtinfo/) Arch Linux package installed.  If this package is not installed, stack will not be able to install GHC.  

If you use the [ArchHaskell repository](https://wiki.archlinux.org/index.php/ArchHaskell), you can also get the `haskell-stack` package from there.

## NixOS

*note*: for 32-bit, use the [generic Linux option](#linux)

1. Clone the git repo:

         git clone https://github.com/commercialhaskell/stack.git

2. Create a `shell.nix` file:

         cabal2nix --shell ./. --no-check > shell.nix

   Note that the tests fail on NixOS, so disable them with `--no-check`.

3. Install stack to your user profile:

         nix-env -i -f shell.nix

## Linux

(64-bit and 32-bit options available)

* Download [the latest release](https://github.com/commercialhaskell/stack/releases/latest).  Note: the `-gmp4` variants are for older distributions (such as CentOS 6.x) that only include libgmp4 (libgmp.so.3)
* Extract the "stack…" executable somewhere on your `$PATH`  (see [Path section below](#path))
* Rename the long filename to just "stack"
* Edit the file to give it permission to be executable (`chmod a+x /path/to/stack`).
* Now you can run `stack` from the terminal.

Tested on Fedora 20: make sure to install the following packages `sudo yum install perl make automake gcc gmp-devel`.
For Gentoo users, make sure to have the `ncurses` package with `USE=tinfo` (without it, stack will not be able to install GHC).

## Path

You can install stack by copying it anywhere on your PATH environment variable. We recommend installing in the same directory where stack itself will install executables (that way stack is able to upgrade itself!). On Windows, that directory is `%APPDATA%\local\bin`, e.g. "c:\Users\Michael\AppData\Roaming\local\bin". For other systems, use `$HOME/.local/bin`.

If you don't have that directory in your PATH, you may need to update your PATH (such as by editing .bashrc).

If you're curious about the choice of these paths, see [issue #153](https://github.com/commercialhaskell/stack/issues/153)

## Upgrade

There are essentially three different approaches to upgrade:

* If you're using a package manager (e.g., the Ubuntu debs listed above) and are happy with sticking with the officially released binaries, simply follow your normal package manager strategies for upgrading (e.g. `apt-get update && apt-get upgrade`).
* If you're not using a package manager but want to stick with the official binaries (such as on Windows or Mac), you'll need to manually follow the steps above to download the newest binaries from the release page and replace the old binary.
* The `stack` tool itself ships with an `upgrade` command, which will build `stack` from source and install it to the default install path (see the previous section). You can use `stack upgrade` to get the latest official release, and `stack upgrade --git` to install from Git and leave on the bleeding edge. If you follow this, make sure that this directory is on your `PATH` and takes precedence over the system installed `stack`. For more information, see [this discussion](https://github.com/commercialhaskell/stack/issues/237#issuecomment-126793301).
